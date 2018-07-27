# Docs

This directory contains some notes and documentation concerning Seashell. 

## Usage

Requires [pandoc](https://pandoc.org/).

	$ make        # generate all HTML files
	$ make x.html # generate x.html (assuming existence of x.md)
	$ make clean  # remove generated HTML files

There is also a configuration for [modd](https://github.com/cortesi/modd), which can automatically rebuild the HTML when you make changes.
