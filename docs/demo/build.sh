#!/bin/bash

set -x
set -e

yarn install

./node_modules/.bin/browserify index.js -o bundle.js

cat index-template.html | \
  sed "s/%%TIME%%/$(date '+%R %b %d, %Y')/g" | \
  sed "s/%%COMMIT%%/$(git rev-parse HEAD)/g" | \
  sed "s/%%COMMIT_SHORT%%/$(git rev-parse --short HEAD)/g" \
  > index.html
