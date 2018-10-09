.PHONY: build test watch demo website

SRCS:=src bin test

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
	- cd website && yarn deploy
	$(MAKE) -C docs deploy
