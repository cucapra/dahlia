FROM ocaml/opam:alpine
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>

# pipenv for running the buildbot.
RUN sudo apk add --no-cache python3
RUN pip3 install --user pipenv
ENV PATH /home/opam/.local/bin:${PATH}
ENV LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

# OCaml dependencies.
RUN opam repo remove default && opam repo add default https://opam.ocaml.org
RUN opam depext -i dune menhir

# Volume, port, and command for buildbot.
VOLUME seashell/buildbot/instance
EXPOSE 8000
CMD ["make", "-C", "buildbot", "serve"]

# Add Seashell source.
ADD --chown=opam:nogroup . seashell
WORKDIR seashell

# Build Seashell.
RUN opam config exec dune build
RUN opam config exec dune install

# Set up buildbot.
RUN cd buildbot ; pipenv install
