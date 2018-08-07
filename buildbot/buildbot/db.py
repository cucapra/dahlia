import tinydb
import threading
import secrets
import time
import os
from contextlib import contextmanager

DB_FILENAME = 'db.json'
JOBS_DIR = 'jobs'
ARCHIVE_NAME = 'code'
CODE_DIR = 'code'

Job = tinydb.Query()


@contextmanager
def chdir(path):
    """Temporarily change the working directory (then change back).
    """
    old_dir = os.getcwd()
    os.chdir(path)
    yield
    os.chdir(old_dir)


class NotFoundError(Exception):
    pass


def log(job, message):
    """Add a message to the job's log.
    """
    job['log'].append((time.time(), message))


class JobDB:
    def __init__(self, base_path):
        self.base_path = base_path
        os.makedirs(self.base_path, exist_ok=True)
        os.makedirs(os.path.join(self.base_path, JOBS_DIR), exist_ok=True)

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
        log(job, 'acquired in state {}'.format(new_state))
        self.jobs.write_back([job])

        return job

    def _add(self, state):
        name = secrets.token_urlsafe(8)
        doc_id = self.jobs.insert({
            'name': name,
            'started': time.time(),
            'state': state,
            'log': [],
        })
        return self.jobs.get(doc_id=doc_id)

    def add(self, state):
        """Add a new job and return it.
        """
        with self.cv:
            job = self._add(state)
            self.cv.notify_all()
        return job

    @contextmanager
    def create(self, start_state, end_state):
        """A context manager for creating initializing a new job. The
        job directory is created, and the working directory is
        temporarily changed there.
        """
        job = self.add(start_state)
        path = self.job_dir(job['name'])
        os.mkdir(path)
        with chdir(path):
            yield job
        self.set_state(job, end_state)

    def set_state(self, job, state):
        """Update a job's state.
        """
        with self.cv:
            job['state'] = state
            log(job, 'state changed to {}'.format(state))
            self.jobs.write_back([job])
            self.cv.notify_all()

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
