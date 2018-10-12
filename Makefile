.PHONY: build website install

SRCS:=src bin test

build:
	dune build && dune install

install:
	opam install dune menhir -y
	opam install . --deps-only -y

# Rsync the docs and the website
website: install
	eval `opam env` && dune build js/seac_js.bc.js
	cp ./_build/default/js/seac_js.bc.js ./website/seashell.js
	cd website && yarn deploy
	$(MAKE) -C docs deploy
