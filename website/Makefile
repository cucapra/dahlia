.PHONY : deploy build

build:
	yarn
	yarn build

develop:
	yarn run start

# Deployment.
RSYNCARGS := --compress --recursive --checksum --itemize-changes \
	--delete -e ssh --perms --chmod=Du=rwx,Dgo=rx,Fu=rw,Fog=r \
	--delete-excluded \
	--exclude .DS_Store --exclude Makefile
DEST := courses:coursewww/capra.cs.cornell.edu/htdocs/fuse
deploy: build
	rsync $(RSYNCARGS) ./build/fuse/ $(DEST)
