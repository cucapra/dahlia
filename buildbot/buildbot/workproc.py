import curio
import os
from . import worker
from . import db
from flask.config import Config
import sys


class WorkProc:
    def __init__(self, basedir):
        # Load the configuration. We're just reusing Flask's simple
        # configuration component here.
        self.config = Config(basedir)
        self.config.from_object('buildbot.config_default')
        self.config.from_pyfile('buildbot.cfg', silent=True)

        # Create the database.
        self.db = db.JobDB(basedir)

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
        while True:
            async for line in client.makefile('rb'):
                # Each line is a job name.
                job_name = line.decode('utf8').strip()
                print(job_name)

                # Just notify the database that something changed.
                with self.db.cv:
                    self.db.cv.notify_all()

    def serve(self, sockpath='workproc.sock'):
        """Start listening on a Unix domain socket for incoming
        messages. Run indefinitely (until the server is interrupted).
        """
        try:
            curio.run(curio.unix_server, sockpath, self.handle)
        except KeyboardInterrupt:
            pass
        finally:
            os.unlink(sockpath)


if __name__ == '__main__':
    p = WorkProc(sys.argv[1])
    p.start()
    p.serve()
