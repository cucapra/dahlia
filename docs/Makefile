.PHONY : all clean deploy

MD   := $(wildcard *.md)
HTML := $(MD:%.md=%.html)

all: $(HTML)

clean:
	rm *.html

%.html: %.md
	pandoc $^ -f markdown -t html -s --katex -o $@

# Deployment.
RSYNCARGS := --compress --recursive --checksum --itemize-changes \
	--delete -e ssh --perms --chmod=Du=rwx,Dgo=rx,Fu=rw,Fog=r \
	--delete-excluded \
	--exclude .DS_Store --exclude Makefile
DEST := courses:coursewww/capra.cs.cornell.edu/htdocs/seashell/docs
deploy: $(HTML)
	rsync $(RSYNCARGS) ./ $(DEST)
