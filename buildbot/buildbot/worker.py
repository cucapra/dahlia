import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR

SEASHELL_EXT = '.ss'
C_EXT = '.c'


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
        with self.db.work('uploaded', 'unpacking', 'unpacked') as job:
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
        with self.db.work('unpacked', 'seashelling', 'seashelled') as job:
            # Look for the Seashell source code.
            code_dir = self.db.job_dir(job['name'])
            for name in os.listdir(code_dir):
                _, ext = os.path.splitext(name)
                if ext == SEASHELL_EXT:
                    source_name = name
                    break
            else:
                self.db._log(job, 'no source file found')
                return

            # Read the source code.
            with open(os.path.join(code_dir, source_name), 'rb') as f:
                code = f.read()

            # Run the Seashell compiler.
            proc = subprocess.run(
                [compiler],
                stdin=code,
                check=True,
                capture_output=True,
            )
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
