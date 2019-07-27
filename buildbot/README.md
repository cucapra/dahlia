Seashell Buildbot
=================

This is a server for building and running Seashell programs on our infrastructure.


Running the Buildbot
--------------------

To set things up, get [pipenv][], and then type `pipenv install`.

To use the "live" browser interface, you will also need to get [Yarn][] (or [npm][]) and type `yarn` then `yarn build` to set up the necessary JavaScript.

The server keeps the data, including the database and the file trees for each job, in an `instance` directory here.

We have different recommendations depending on whether you're running the buildbot locally (for development) or on a proper server.

### Development

Then, run this command to get a development server:

    $ FLASK_APP=buildbot.server FLASK_ENV=development pipenv run flask run

You can also use `make dev` as a shortcut.
This route automatically starts the necessary worker threads in the same process as the development server.

### Deployment

There are two differences in deployment: you'll want to use a proper server, and the buildbot will want to spawn a separate process just for the worker threads.

Use this command to start the workers:

    $ pipenv run python -m buildbot.workproc

For the server, [Gunicorn][] is a good choice (and included in the dependencies). Here's how you might invoke it:

    $ pipenv run gunicorn buildbot.server:app

The `make serve` target does that.

The two processes communicate through a Unix domain socket in the instance directory.
You can provide a custom instance directory path to the workproc invocation as an argument.

[gunicorn]: http://gunicorn.org
[pipenv]: http://pipenv.org
[yarn]: https://yarnpkg.com/en/
[npm]: http://npmjs.com


Using the Buildbot
------------------

There is a [browser interface](http://gorgonzola.cs.cornell.edu:8000/) that lets you view jobs and start new ones.
There's even a hacky interface for compiling code interactively.
It's also possible to do everything from the command line using [curl][].

To submit a job, upload a file to the `/jobs` endpoint:

    $ curl -F file=@foo.zip $BUILDBOT/jobs

For example, you can zip up a directory and submit it like this:

    $ zip -r - . | curl -F file='@-;filename=code.zip' $BUILDBOT/jobs

If the directory contains data files with `.data` extension, they'll be copied over to the target FPGA.

You can also specify job configuration options as further POST parameters:

- `skipseashell`, which lets you supply plain HLS C code as input.
- `estimate`, to use the Xilinx toolchain's resource estimation facility. The job will skip synthesis and execution on the FPGA.
- `skipexec`, to avoid actually trying to run the generated program. (Only necessary when `estimate` is false---estimate runs skip execution by default.)
- `make`, to use a Makefile instead of the built-in compilation workflow (see below).
- `hwname`, which lets you provide a name for the job during makefile flow.
- `directives`, which lets you provide the name of a file with a set of directives (pragmas).

Use `-F <option>=1` to enable these options with `curl`.

To see a list of the current jobs, get `/jobs.csv`:

    $ curl $BUILDBOT/jobs.csv

To get details about a specific job, request `/jobs/<name>`:

    $ curl $BUILDBOT/jobs/d988ruiuAk4

You can also download output files from a job:

    $ curl -O $BUILDBOT/jobs/d988ruiuAk4/files/code/compiled.o

There is also a JSON list of all the files at `/jobs/$ID/files`.


Makefiles
---------

Larger projects that use multiple sources and need them to linked in a particular fashion should use the `make` configuration option. With this option, buildbot will run the provided Makefile instead of running its own commands and assume that the artifact is built when the command terminates successfully.

For estimation, buildbot supplies flags for `sds++` using the `SDSFLAGS` variable. In your Makefile, make sure that you pass in this option when building hardware targets:

```make
%.o: %.c
    sds++ $(SDSFLAGS) $< -o $@
```
For configurations provided above, buildbot also relies on following variables, `ESTIMATE`, `DIRECTIVES`. Make sure these variables behave according to the configuration description in your makefile.

[curl]: https://curl.haxx.se
