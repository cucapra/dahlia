.PHONY: build website install

build:
	dune build && dune install

install:
	sbt compile

# Rsync the docs and the website
website:
	$(MAKE) -C notes deploy
