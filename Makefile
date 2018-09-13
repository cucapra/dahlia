.PHONY: build test watch

SRCS:=src bin test

build:
	dune build && dune install

test:
	$(MAKE) && dune runtest

watch:
	find $(SRCS) | entr -cp make test
