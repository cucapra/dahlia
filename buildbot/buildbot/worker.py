import threading
import tinydb


class WorkThread(threading.Thread):
    def __init__(self, db):
        self.db = db
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            job = self.db._acquire('uploaded', 'unpacking')
            print('unpacking', job['name'])
