#!/bin/bash

set -x
set -e

./node_modules/.bin/browserify index.js -o dist/bundle.js

cat index-template.html | \
  sed "s/%%COMMIT%%/$(git rev-parse HEAD)/g" | \
  sed "s/%%COMMIT_SHORT%%/$(git rev-parse --short HEAD)/g" \
  > dist/index.html
