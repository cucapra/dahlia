.PHONY: build test watch

SRCS:=src bin test

build:
	dune build && dune install

test:
	dune runtest

watch:
	find $(SRCS) | entr -cp make
