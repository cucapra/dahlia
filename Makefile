.PHONY: build test watch demo website

SRCS:=src bin test

install:
	set -x
	opam install dune menhir -y
	opam install . --deps-only -y
	eval $(opam env)
	which dune

build:
	dune build && dune install

test:
	$(MAKE) && dune runtest

watch:
	dune runtest -w

demo:
	dune build js/seac_js.bc.js
	cp ./_build/default/js/seac_js.bc.js ./docs/demo/seashell.js

# Rsync the docs and the website
website:
	git clean -fd # Remove untracked files
	$(MAKE) install
	$(MAKE) demo
	cd website && yarn deploy
	$(MAKE) -C docs deploy
