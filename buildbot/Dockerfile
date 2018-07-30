FROM ocaml/opam:debian-9_ocaml-4.06.0_flambda
MAINTAINER Adrian Sampson <asampson@cs.cornell.edu>

# pipenv for running the buildbot.
RUN sudo apt-get install -y python3-pip
RUN pip3 install --user pipenv

# OCaml dependencies.
RUN opam update
RUN opam depext -i dune menhir

# Get Seashell source.
RUN git clone https://github.com/cucapra/seashell.git
WORKDIR seashell

# Build Seashell.
RUN dune build
RUN dune install
