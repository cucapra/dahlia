.PHONY: build test watch demo

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
	cd ./docs/demo && yarn build
