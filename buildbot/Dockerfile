FROM ocaml/opam:alpine
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>

# pipenv for running the buildbot.
RUN sudo apk add --no-cache python3
RUN pip3 install --user pipenv

# OCaml dependencies.
RUN opam repo remove default && opam repo add default https://opam.ocaml.org
RUN opam depext -i dune menhir

# Get Seashell source.
RUN git clone https://github.com/cucapra/seashell.git
WORKDIR seashell

# Build Seashell.
RUN opam config exec dune build
RUN opam config exec dune install
