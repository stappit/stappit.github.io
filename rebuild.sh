#!/bin/sh

stack build && stack exec site clean && git submodule add https://github.com/stappit/stappit.github.io _site  && stack exec site build
