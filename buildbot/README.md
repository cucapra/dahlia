Seashell Buildbot
=================

This is a server for building and running Seashell programs on our infrastructure.


Running the Buildbot
--------------------

To run your own server, get [pipenv][], then type `pipenv install`.
Then, to run a local server:

    $ FLASK_APP=buildbot pipenv run flask run

You can also use `make dev` as a shortcut (with debugging enabled).
Also, `make serve` runs a production server using [Gunicorn][].

The server keeps the data, including the database and the file trees for each job, in an `instance` directory here.

To use the "live" browser interface, you will also need to get [Yarn][] (or [npm][]) and type `yarn` then `yarn build` to set up the necessary JavaScript.

[gunicorn]: http://gunicorn.org
[pipenv]: http://pipenv.org
[yarn]: https://yarnpkg.com/en/
[npm]: http://npmjs.com


Using the Buildbot
------------------

There is a browser interface that lets you view jobs and start new ones.
There's even a hacky interface for compiling code interactively.
It's also possible to do everything from the command line using [curl][].

To submit a job, upload a file to the `/jobs` endpoint:

    $ curl -F file=@foo.zip $BUILDBOT/jobs

For example, you can zip up a directory and submit it like this:

    $ zip -r - . | curl -F file='@-;filename=code.zip' $BUILDBOT/jobs

It's also possible to provide Seashell code as an ordinary POST string instead of as a file attachment using the `code` parameter.

To see a list of the current jobs, get `/jobs.csv`:

    $ curl $BUILDBOT/jobs.csv

To get details about a specific job, request `/jobs/<name>`:

    $ curl $BUILDBOT/jobs/d988ruiuAk4

You can also download output files from a job:

    $ curl -O $BUILDBOT/jobs/d988ruiuAk4/files/code/compiled.o

[curl]: https://curl.haxx.se
