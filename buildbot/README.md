Seashell Buildbot
=================

This is a server for building and running Seashell programs on our infrastructure.

Get [pipenv][], then type `pipenv install`.
Then, to run a local server:

    $ FLASK_APP=buildbot.py pipenv run flask run

To submit a job, upload a file to the `/jobs` endpoint:

    $ curl -F file=@foo.zip $BUILDBOT/jobs

To see a list of the current jobs, get `/jobs.csv`:

    $ curl $BUILDBOT/jobs.csv

[pipenv]: http://pipenv.org
