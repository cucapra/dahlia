#!/bin/sh

set -x
set -e

DEST=courses:coursewww/capra.cs.cornell.edu/htdocs/seashell/

rsync --compress --recursive --checksum --itemize-changes \
	--delete -e ssh --perms --chmod=Du=rwx,Dgo=rx,Fu=rw,Fog=r \
	--exclude docs ./dist/ $DEST
