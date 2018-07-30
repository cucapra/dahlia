import threading
import subprocess
from .db import ARCHIVE_NAME, CODE_DIR


class WorkThread(threading.Thread):
    def __init__(self, db):
        self.db = db
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            job = self.db.acquire('uploaded', 'unpacking')
            subprocess.run(
                ["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)],
                cwd=self.db.job_dir(job['name']),
            )
            self.db.set_state(job['name'], 'unpacked')
