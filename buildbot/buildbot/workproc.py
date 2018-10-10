import curio
import os
from . import worker
from .db import JobDB
from flask.config import Config
import sys


INSTANCE_DIR = 'instance'
SOCKNAME = 'workproc.sock'


class WorkProc:
    """A container process for our worker threads that can receive
    notifications from a Unix domain socket.
    """

    def __init__(self, basedir, db=None):
        """Create a container using a given base directory for the
        storage and socket. Optionally, provide a database object to use
        that instead of creating a new one (to, for example, reuse its
        internal locks).
        """
        self.basedir = os.path.abspath(basedir)

        # Load the configuration. We're just reusing Flask's simple
        # configuration component here.
        self.config = Config(self.basedir)
        self.config.from_object('buildbot.config_default')
        self.config.from_pyfile('buildbot.cfg', silent=True)

        # Create the database.
        self.db = db or JobDB(self.basedir)

    def start(self):
        """Create and start the worker threads.
        """
        threads = worker.work_threads(self.db, self.config)
        for thread in threads:
            if not thread.is_alive():
                thread.start()

    async def handle(self, client, addr):
        """Handle an incoming socket connection.
        """
        async for line in client.makefile('rb'):
            # Each line is a job name.
            job_name = line.decode('utf8').strip()
            print(job_name)

            # Just notify the database that something changed.
            with self.db.cv:
                self.db.cv.notify_all()

    def serve(self):
        """Start listening on a Unix domain socket for incoming
        messages. Run indefinitely (until the server is interrupted).
        """
        sockpath = os.path.join(self.basedir, SOCKNAME)
        if os.path.exists(sockpath):
            os.unlink(sockpath)
        try:
            curio.run(curio.unix_server, sockpath, self.handle)
        except KeyboardInterrupt:
            pass
        finally:
            os.unlink(sockpath)


def notify(basedir, jobname):
    """Notify a running workproc that a new job has been added to the
    database (via the socket in basedir).
    """
    curio.run(_notify, basedir, jobname)


async def _notify(basedir, jobname):
    sockpath = os.path.join(basedir, SOCKNAME)
    line = (jobname + '\n').encode('utf8')
    sock = await curio.open_unix_connection(sockpath)
    await sock.makefile('wb').write(line)
    await sock.close()


if __name__ == '__main__':
    p = WorkProc(sys.argv[1] if len(sys.argv) > 1 else INSTANCE_DIR)
    p.start()
    p.serve()
