#!/bin/sh

GIT=_site/.git
TMP=_site_git_tmp

mv $GIT $TMP
stack build
stack exec site rebuild
mv $TMP $GIT
