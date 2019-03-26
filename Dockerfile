FROM frolvlad/alpine-scala
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>
MAINTAINER Rachit Nigam <rnigam@cs.cornell.edu>

# Add sbt for compiling the compiler.
ENV SBT_VERSION 1.2.8
RUN apk update && \
    apk add bash curl
RUN cd /opt && \
    curl -LO "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" && \
    tar xf sbt-${SBT_VERSION}.tgz && \
    ln -s /opt/sbt/bin/sbt /bin && \
    rm sbt-${SBT_VERSION}.tgz && \
    sbt version

# Add Python, pipenv, and node for buildbot. The buildbot also needs OpenSSH
# and the sshpass utility for its Zynq execution stage.
RUN apk add python3 py3-pip nodejs-current yarn \
    openssh sshpass git make g++
RUN pip3 install pipenv

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
ENV PATH /seashell:${PATH}

# Set up buildbot.
RUN cd buildbot ; PIPENV_PIPFILE= pipenv install
RUN cd buildbot ; yarn
RUN cd buildbot ; yarn build
