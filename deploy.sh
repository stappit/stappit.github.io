#!/bin/sh

set -e

# make sure the submodule is loaded
rm -rf _site
git config -f .gitmodules --get-regexp '^submodule._site.url*' | read urlkey url
git submodule add --force $url _site
git reset _site

# build the changes
stack exec site build

# publish the changes
cd _site
git add -A
git commit -m "Publish"
git push
cd ..
