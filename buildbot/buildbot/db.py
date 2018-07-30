import tinydb
import threading
import secrets
import time
import os

DB_FILENAME = 'db.json'
JOBS_DIR = 'jobs'
ARCHIVE_NAME = 'code'
CODE_DIR = 'code'

Job = tinydb.Query()


class NotFoundError(Exception):
    pass


class JobDB:
    def __init__(self, base_path):
        self.base_path = base_path
        self.db = tinydb.TinyDB(os.path.join(base_path, DB_FILENAME))
        self.jobs = self.db.table('jobs')

        self.cv = threading.Condition()


    def _count(self, state):
        """Get the number of jobs in the database in a given state.
        """
        return len(self.jobs.search(tinydb.Query().state == state))

    def _get(self, name):
        """Get a job by its name.

        Raise a NotFoundError if there is no such job.
        """
        matches = self.jobs.search(tinydb.Query().name == name)
        if not matches:
            raise NotFoundError()
        assert len(matches) == 1
        return matches[0]

    def _acquire(self, old_state, new_state):
        """Look for a job in `old_state`, update it to `new_state`, and
        return it.

        Raise a `NotFoundError` if there is no such job.
        """
        matches = self.jobs.search(tinydb.Query().state == old_state)
        if not matches:
            raise NotFoundError()
        job = matches[0]

        job['state'] = new_state
        self.jobs.write_back([job])

        return job

    def _add(self, state):
        name = secrets.token_urlsafe(8)
        self.jobs.insert({
            'name': name,
            'started': time.time(),
            'state': state,
        })
        return name

    def add(self, state):
        """Add a new job and return its name.
        """
        with self.cv:
            name = self._add(state)
            self.cv.notify()
        return name

    def set_state(self, name, state):
        """Update a job's state.
        """
        with self.cv:
            job = self._get(name)
            job['state'] = state
            self.jobs.write_back([job])
            self.cv.notify()

    def acquire(self, old_state, new_state):
        """Block until a job is available in `old_state`, update its
        state to `new_state`, and return it.
        """
        with self.cv:
            while not self._count(old_state):
                self.cv.wait()
            job = self._acquire(old_state, new_state)
            return job

    def get(self, name):
        """Get the job with the given name.
        """
        with self.cv:
            return self._get(name)

    def job_dir(self, job_name):
        """Get the path to a job's work directory.
        """
        return os.path.join(self.base_path, JOBS_DIR, job_name)
