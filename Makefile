.PHONY: build test

build:
	dune build && dune install

test:
	$(MAKE) -C examples/
