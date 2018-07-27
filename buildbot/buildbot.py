import flask
import tinydb

# Our Flask application.
app = flask.Flask(__name__)

# Configuration. We include some defaults and allow overrides.
app.config.from_pyfile('buildbot.base.cfg')
app.config.from_pyfile('buildbot.site.cfg', silent=True)

# Connect to our database.
db = tinydb.TinyDB(app.config['DATABASE'])
jobs = db.table('jobs')


@app.route('/')
def list_jobs():
    out = []
    for job in jobs:
        out.append(job['name'])
    return '\n'.join(out), 200, {'Content-Type': 'text/plain'}
