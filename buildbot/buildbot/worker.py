import threading
import tinydb


class WorkThread(threading.Thread):
    def __init__(self, db):
        self.db = db
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            with self.db.cv:
                while not self.db._count('uploaded'):
                    self.db.cv.wait()
                job = self.db._acquire('uploaded', 'unpacking')
                print(job)
