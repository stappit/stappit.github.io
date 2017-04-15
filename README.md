# My Hakyll Blog

This is the haskell/markdown source for my hakyll blog.


## Structure

The source for the blog is in the `hakyll` branch.  The master branch is included in the hakyll branch as a submodule called `_site`.  This allows us to build to the `_site` folder (hakyll default), then commit/push changes to master from within that folder.

The main difficulty arises with a hakyll `rebuild`, as this deletes the `_site` directory then recreates it without the `.git` subdirectory.  If we simply re-add the submodule, we confuse hakyll by losing the modified dates.

As a workaround, we use a [rebuild.sh](.publish.sh) to copy the `.git` directory before running a `rebuild`, making sure to replace it afterwards. We can then publish with [publish.sh](.publish.sh).
