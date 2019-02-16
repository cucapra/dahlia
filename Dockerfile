FROM hseeberger/scala-sbt
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>
MAINTAINER Rachit Nigam <rnigam@cs.cornell.edu>

# Add Python, pipenv, and node for buildbot.
RUN apt-get install -y software-properties-common && \
    add-apt-repository ppa:deadsnakes/ppa && \
    curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - && \
    apt-add-repository \
        'deb https://dl.yarnpkg.com/debian/ stable main' && \
    apt-get update
RUN apt-get install -y python3.7 nodejs yarn
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.7
ENV PATH ${HOME}/.local/bin:${PATH}
RUN pip install --user pipenv

# Volume, port, and command for buildbot.
VOLUME ${HOME}/seashell/buildbot/instance
EXPOSE 8000
ENV PIPENV_PIPFILE=buildbot/Pipfile
CMD ["pipenv", "run", \
     "gunicorn", "--bind", "0.0.0.0:8000", "--chdir", "buildbot", \
     "buildbot.server:app"]

# Add Seashell source.
ADD . /seashell
WORKDIR /seashell

# Build Seashell.
RUN sbt assembly
ENV PATH ${PWD}

# Avoids a bug in a recent version of pip:
# https://github.com/pypa/pipenv/issues/2924
RUN pip install pip==18.0
RUN cd buildbot ; PIPENV_PIPFILE= pipenv run pip install pip==18.0

# Set up buildbot.
RUN cd buildbot ; PIPENV_PIPFILE= pipenv install
RUN cd buildbot ; yarn
RUN cd buildbot ; yarn build
