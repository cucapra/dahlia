import threading
import subprocess
from .db import ARCHIVE_NAME, CODE_DIR


class WorkThread(threading.Thread):
    def __init__(self, db):
        self.db = db
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            with self.db.work('uploaded', 'unpacking', 'unpacked') as job:
                proc = subprocess.run(
                    ["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)],
                    cwd=self.db.job_dir(job['name']),
                    check=True,
                    capture_output=True,
                )
                self.db._log(job, proc.stdout.decode('utf8', 'ignore'))
