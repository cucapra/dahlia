.PHONY: build website install

build:
	sbt assembly

install:
	sbt compile

# Rsync the docs and the website
website:
	-$(MAKE) -C notes deploy
	-$(MAKE) -C website deploy
