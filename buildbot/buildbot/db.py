import threading
import secrets
import time
import os
from contextlib import contextmanager
import json

JOBS_DIR = 'jobs'
ARCHIVE_NAME = 'code'
CODE_DIR = 'code'
INFO_FILENAME = 'info.json'


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

        self.cv = threading.Condition()

    def job_dir(self, job_name):
        """Get the path to a job's work directory.
        """
        return os.path.join(self.base_path, JOBS_DIR, job_name)

    def _info_path(self, name):
        """Get the path to a job's info JSON file."""
        return os.path.join(self.job_dir(name), INFO_FILENAME)

    def _read(self, name):
        """Read a job from its info file.

        Raise a NotFoundError if there is no such job.
        """
        path = self._info_path(name)
        if os.path.isfile(path):
            with open(path) as f:
                return json.load(f)
        else:
            raise NotFoundError()

    def _write(self, job):
        """Write a job back to its info file.
        """
        with open(self._info_path(job['name']), 'w') as f:
            json.dump(job, f)

    def _all(self):
        """Read all the jobs.

        This is probably pretty slow, and its O(n) where n is the total
        number of jobs in the system.
        """
        for name in os.listdir(os.path.join(self.base_path, JOBS_DIR)):
            path = self._info_path(name)
            if os.path.isfile(path):
                with open(path) as f:
                    yield json.load(f)

    def _acquire(self, old_state, new_state):
        """Look for a job in `old_state`, update it to `new_state`, and
        return it.

        Raise a `NotFoundError` if there is no such job.
        """
        for job in self._all():
            if job['state'] == old_state:
                break
        else:
            raise NotFoundError()

        job['state'] = new_state
        log(job, 'acquired in state {}'.format(new_state))
        with open(self._info_path(job['name']), 'w') as f:
            json.dump(job, f)

        return job

    def _add(self, state):
        name = secrets.token_urlsafe(8)
        job = {
            'name': name,
            'started': time.time(),
            'state': state,
            'log': [],
        }
        os.mkdir(self.job_dir(name))
        self._write(job)
        return job

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
        with chdir(path):
            yield job
        self.set_state(job, end_state)

    def set_state(self, job, state):
        """Update a job's state.
        """
        with self.cv:
            job['state'] = state
            log(job, 'state changed to {}'.format(state))
            self._write(job)
            self.cv.notify_all()

    def acquire(self, old_state, new_state):
        """Block until a job is available in `old_state`, update its
        state to `new_state`, and return it.
        """
        with self.cv:
            while True:
                try:
                    job = self._acquire(old_state, new_state)
                except NotFoundError:
                    pass
                else:
                    break
                self.cv.wait()
            return job

    def get(self, name):
        """Get the job with the given name.
        """
        with self.cv:
            return self._read(name)
