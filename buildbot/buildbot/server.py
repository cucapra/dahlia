import flask
from flask import request
import tinydb
import secrets
import os
import time
from io import StringIO
import csv
from . import worker

# Our Flask application. Our "instance path," where we search for
# configuration fails and such, is the parent directory of our Python
# package (so this server can be run "in place.")
app = flask.Flask(__name__, instance_relative_config=True,
                  instance_path=os.path.dirname(os.path.dirname(__file__)))

# Configuration. We include some defaults and allow overrides.
app.config.from_pyfile('buildbot.base.cfg')
app.config.from_pyfile('buildbot.site.cfg', silent=True)

# Connect to our database.
db = tinydb.TinyDB(os.path.join(app.instance_path, app.config['DATABASE']))
jobs = db.table('jobs')

# Create our worker thread (but don't start it yet).
work_thread = worker.WorkThread(jobs)


def job_dir(job_name):
    """Get the path to a job's work directory.
    """
    return os.path.join(app.instance_path, app.config['JOBS_DIR'], job_name)


@app.route('/jobs', methods=['POST'])
def add_job():
    if 'file' not in request.files:
        return 'missing file', 400
    file = request.files['file']

    # Check that the file has an allowed extension.
    _, ext = os.path.splitext(file.filename)
    if ext[1:] not in app.config['UPLOAD_EXTENSIONS']:
        return 'invalid extension {}'.format(ext), 400

    # Generate a random name for the job.
    job_name = secrets.token_urlsafe(8)

    # Create the job's directory and save the code there.
    path = job_dir(job_name)
    os.makedirs(path)
    archive_path = os.path.join(path, app.config['ARCHIVE_NAME'] + ext)
    file.save(archive_path)

    # Create a job record.
    with worker.jobs_cv:
        jobs.insert({
            'name': job_name,
            'started': time.time(),
            'state': 'uploaded',
        })
        worker.jobs_cv.notify()

    # Lazily start the worker thread, if we haven't already.
    if not work_thread.is_alive():
        work_thread.start()

    return job_name


@app.route('/jobs.csv')
def jobs_csv():
    output = StringIO()
    writer = csv.DictWriter(
        output,
        ['name', 'started', 'state'],
    )
    writer.writeheader()

    for job in jobs:
        writer.writerow(job)

    csv_data = output.getvalue()
    return csv_data, 200, {'Content-Type': 'text/csv'}


@app.route('/jobs/<name>')
def get_job(name):
    matches = jobs.search(tinydb.Query().name == name)
    if not matches:
        return 'job not found', 404
    assert len(matches) == 1
    job = matches[0]

    return flask.jsonify(job)
