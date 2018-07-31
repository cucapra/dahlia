FROM python:3.7-alpine
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>

# Add pipenv for buildbot.
RUN pip install pipenv

# Add OCaml and enough dependencies to build OCaml packages.
RUN apk add --no-cache opam ocaml-compiler-libs bash m4 build-base
RUN opam init -y

# Our OCaml dependencies. We already have ocamlbuild, so we have a workaround:
# https://github.com/ocaml/ocamlbuild/issues/109
ENV CHECK_IF_PREINSTALLED=false
RUN opam install dune menhir

# Volume, port, and command for buildbot.
VOLUME seashell/buildbot/instance
EXPOSE 8000
CMD ["make", "-C", "buildbot", "serve"]

# Add Seashell source.
ADD . seashell
WORKDIR seashell

# Build Seashell.
RUN opam config exec -- dune build
RUN opam config exec -- dune install

# Set up buildbot.
RUN cd buildbot ; pipenv install
