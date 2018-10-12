# Docs

This directory contains some notes and documentation concerning Seashell. You can build these yourself or view all of them rendered [here](https://capra.cs.cornell.edu/seashell/docs/).

## Usage

Requires [pandoc](https://pandoc.org/).

	$ make        # generate all HTML files
	$ make x.html # generate x.html (assuming existence of x.md)
	$ make clean  # remove generated HTML files

There is also a configuration for [modd](https://github.com/cortesi/modd), which can automatically rebuild the HTML when you make changes.

## Serving the website

The built directory is served using the `make deploy`. Make changes to it as needed.
