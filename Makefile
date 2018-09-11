.PHONY: build test watch

SRCS:=src bin test

build:
	dune build && dune install

test:
	$(MAKE) -C examples/

watch:
	find $(SRCS) | entr -cp make
