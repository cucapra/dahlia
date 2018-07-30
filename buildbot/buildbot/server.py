import flask
from flask import request
import os
from io import StringIO
import csv
from . import worker
from .db import JobDB, ARCHIVE_NAME

# Our Flask application. Our "instance path," where we search for
# configuration fails and such, is the parent directory of our Python
# package (so this server can be run "in place.")
app = flask.Flask(__name__, instance_relative_config=True,
                  instance_path=os.path.dirname(os.path.dirname(__file__)))

# Configuration. We include some defaults and allow overrides.
app.config.from_pyfile('buildbot.base.cfg')
app.config.from_pyfile('buildbot.site.cfg', silent=True)

# Connect to our database.
db = JobDB(app.instance_path)

# Create our worker thread (but don't start it yet).
work_thread = worker.WorkThread(db)


@app.route('/jobs', methods=['POST'])
def add_job():
    if 'file' not in request.files:
        return 'missing file', 400
    file = request.files['file']

    # Check that the file has an allowed extension.
    _, ext = os.path.splitext(file.filename)
    if ext[1:] not in app.config['UPLOAD_EXTENSIONS']:
        return 'invalid extension {}'.format(ext), 400

    # Create a job record.
    job = db.add('uploading')

    # Create the job's directory and save the code there.
    path = db.job_dir(job['name'])
    os.makedirs(path)
    archive_path = os.path.join(path, ARCHIVE_NAME + ext)
    file.save(archive_path)

    # Mark it as uploaded.
    db.set_state(job, 'uploaded')

    # Lazily start the worker thread, if we haven't already.
    if not work_thread.is_alive():
        work_thread.start()

    return job['name']


@app.route('/jobs.csv')
def jobs_csv():
    output = StringIO()
    writer = csv.DictWriter(
        output,
        ['name', 'started', 'state'],
    )
    writer.writeheader()

    for job in db.jobs:
        writer.writerow(job)

    csv_data = output.getvalue()
    return csv_data, 200, {'Content-Type': 'text/csv'}


@app.route('/jobs/<name>')
def get_job(name):
    job = db.get(name)
    return flask.jsonify(job)
