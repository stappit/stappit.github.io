#!/bin/sh

set -e

stack exec site build

# publish the changes
cd _site
git add -A
git commit -m "Publish"
git push
cd ..
