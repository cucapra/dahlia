FROM ocaml/opam2:ubuntu-18.04
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>

# Add Python and pipenv for buildbot.j
RUN sudo apt-get install -y software-properties-common && \
    sudo add-apt-repository ppa:deadsnakes/ppa && \
    sudo apt-get update && \
    sudo apt-get install -y python3.7
RUN curl https://bootstrap.pypa.io/get-pip.py | sudo -H python3.7
ENV PATH ${HOME}/.local/bin:${PATH}
RUN pip install --user pipenv

# Install some of our OCaml dependencies.
RUN opam config exec -- opam depext --install -y dune menhir core.v0.10.0

# Add opam bin directory to our $PATH so we can run seac.
ENV PATH /root/.opam/system/bin:${PATH}

# Volume, port, and command for buildbot.
VOLUME seashell/buildbot/instance
EXPOSE 8000
ENV PIPENV_PIPFILE=buildbot/Pipfile
CMD ["pipenv", "run", \
     "gunicorn", "--bind", "0.0.0.0:8000", "--chdir", "buildbot", \
     "buildbot.server:app"]

# Add Seashell source.
ADD . seashell
WORKDIR seashell

# Build Seashell.
RUN opam install .
RUN eval `opam config env` ; dune build
RUN eval `opam config env` ; dune install

# Set up buildbot.
RUN cd buildbot ; PIPENV_PIPFILE= pipenv install
RUN cd buildbot ; yarn
RUN cd buildbot ; yarn build
