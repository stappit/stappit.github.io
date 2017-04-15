#!/bin/sh

GIT=_site/.git
TMP=_site_git_tmp

mv $GIT $TMP
stack exec site rebuild
mv $TMP $GIT
