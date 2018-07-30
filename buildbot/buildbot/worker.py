import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR
from contextlib import contextmanager

SEASHELL_EXT = '.ss'
C_EXT = '.c'


@contextmanager
def work(db, old_state, temp_state, done_state):
    """A context manager for acquiring a job temporarily in an
    exclusive way to work on it.
    """
    job = db.acquire(old_state, temp_state)
    try:
        yield job
    except Exception as exc:
        db._log(job, traceback.format_exc())
        db.set_state(job, 'failed')
    else:
        db.set_state(job, done_state)


class WorkThread(threading.Thread):
    """A base class for all our worker threads, which run indefinitely
    to process tasks in an appropriate state.
    """

    def __init__(self, db, config):
        self.db = db
        self.config = config
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            self.work()


class UnpackThread(WorkThread):
    """Unpack source code.
    """
    def work(self):
        with work(self.db, 'uploaded', 'unpacking', 'unpacked') as job:
            proc = subprocess.run(
                ["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)],
                cwd=self.db.job_dir(job['name']),
                check=True,
                capture_output=True,
            )
            self.db._log(job, proc.stdout.decode('utf8', 'ignore'))


class SeashellThread(WorkThread):
    """Compile Seashell code to HLS.
    """
    def work(self):
        compiler = self.config["SEASHELL_COMPILER"]
        with work(self.db, 'unpacked', 'seashelling', 'seashelled') as job:
            # Look for the Seashell source code.
            code_dir = os.path.join(self.db.job_dir(job['name']), CODE_DIR)
            for name in os.listdir(code_dir):
                _, ext = os.path.splitext(name)
                if ext == SEASHELL_EXT:
                    source_name = name
                    break
            else:
                self.db.set_state(job, 'failed')
                self.db._log(job, 'no source file found')
                return

            # Read the source code.
            with open(os.path.join(code_dir, source_name), 'rb') as f:
                code = f.read()

            # Run the Seashell compiler.
            try:
                proc = subprocess.run(
                    [compiler],
                    input=code,
                    check=True,
                    capture_output=True,
                )
            except subprocess.CalledProcessError as exc:
                self.db.set_state(job, 'failed')
                msg = 'seac failed ({}):\n{}'.format(
                    exc.returncode,
                    '\n---\n'.join(filter(lambda x: x, (
                        exc.stdout.decode('utf8', 'ignore'),
                        exc.stderr.decode('utf8', 'ignore'),
                    )))
                )
                self.db._log(job, msg)
                return
            self.db._log(job, proc.stderr.decode('utf8', 'ignore'))
            hls_code = proc.stdout

            # Write the C code.
            base, _ = os.path.splitext(source_name)
            with open(os.path.join(code_dir, base + C_EXT), 'wb') as f:
                f.write(hls_code)


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    return [
        UnpackThread(db, config),
        SeashellThread(db, config),
    ]
