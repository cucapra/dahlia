import threading
import tinydb

jobs_cv = threading.Condition()


def count_jobs(jobs, state):
    """Get the number of jobs in the database in a given state.
    """
    return len(jobs.search(tinydb.Query().state == state))


def advance_job(jobs, old_state, new_state):
    """Look for a job in `old_state`, update it to `new_state`, and
    return it.

    Raise a `ValueError` if there is no such job.
    """
    matches = jobs.search(tinydb.Query().state == old_state)
    if not matches:
        raise ValueError
    job = matches[0]

    job['state'] = new_state
    jobs.write_back([job])

    return job


class WorkThread(threading.Thread):
    def __init__(self, jobs):
        self.jobs = jobs
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            with jobs_cv:
                while not count_jobs(self.jobs, 'uploaded'):
                    jobs_cv.wait()
                job = advance_job(self.jobs, 'uploaded', 'unpacking')
                print(job)
