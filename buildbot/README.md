Seashell Buildbot
=================

This is a server for building and running Seashell programs on our infrastructure.

Get [pipenv][], then type `pipenv install`.
Then, to run a local server:

    $ FLASK_APP=buildbot pipenv run flask run

To submit a job, upload a file to the `/jobs` endpoint:

    $ curl -F file=@foo.zip $BUILDBOT/jobs

For example, you can zip up a directory and submit it like this:

    $ zip -r - . | curl -F file='@-;filename=code.zip' $BUILDBOT/jobs

To see a list of the current jobs, get `/jobs.csv`:

    $ curl $BUILDBOT/jobs.csv

To get details about a specific job, request `/jobs/<name>`:

    $ curl $BUILDBOT/jobs/d988ruiuAk4

There is also a browser interface that lets you view jobs and start new ones.

The server keeps the data, including the database and the file trees for each job, in an `instance` directory here.

[pipenv]: http://pipenv.org
