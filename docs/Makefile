.PHONY : all clean deploy

MD   := $(wildcard *.md)
HTML := $(MD:%.md=%.html)

# Some resources for building documents with Pandoc: a nice stylesheet someone
# made for Pandoc documents, and a CDN-hosted KaTeX.
CSS_URL := https://gist.githubusercontent.com/killercup/5917178/raw/40840de5352083adb2693dc742e9f75dbb18650f/pandoc.css
KATEX_URL := https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0/

all: $(HTML)

clean:
	rm *.html style.css

%.html: %.md style.css
	pandoc $< -f markdown -t html -s --katex $(KATEX_URL) -c style.css -o $@

style.css:
	curl -o $@ $(CSS_URL)

# Deployment.
RSYNCARGS := --compress --recursive --checksum --itemize-changes \
	--delete -e ssh --perms --chmod=Du=rwx,Dgo=rx,Fu=rw,Fog=r \
	--delete-excluded \
	--exclude .DS_Store --exclude Makefile
DEST := courses:coursewww/capra.cs.cornell.edu/htdocs/seashell/docs
deploy: $(HTML)
	rsync $(RSYNCARGS) ./ $(DEST)
