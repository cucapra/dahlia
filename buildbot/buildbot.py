import flask
from flask import request
import tinydb
import secrets
import os
import time

# Our Flask application.
app = flask.Flask(__name__)

# Configuration. We include some defaults and allow overrides.
app.config.from_pyfile('buildbot.base.cfg')
app.config.from_pyfile('buildbot.site.cfg', silent=True)

# Connect to our database.
db = tinydb.TinyDB(app.config['DATABASE'])
jobs = db.table('jobs')


def job_dir(job_name):
    """Get the path to a job's work directory.
    """
    return os.path.join(app.config['JOBS_DIR'], job_name)


@app.route('/')
def list_jobs():
    out = []
    for job in jobs:
        out.append(job['name'])
    return '\n'.join(out), 200, {'Content-Type': 'text/plain'}


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
    jobs.insert({
        'name': job_name,
        'started': time.time(),
        'status': 'uploaded',
    })

    return 'job accepted'
