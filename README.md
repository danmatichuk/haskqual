# haskqual - tools for qualified cross-reference lookups in haskell #

# Installation #

Requires the fork of `fast-tags` located in `submodules/fast-tags` to be installed.

Load the given emacs file `haskqual.el` with `load-file`.

Move tags-cached.sh and tags-watcher.sh to somewhere in your PATH.

# Requirements

`tags-watcher.sh` requires `entr` to be installed.

# Usage #

Generate an emacs TAGS file and cache by executing `tags-cached.sh` in the root
of a haskell project. This will create a TAGS-cache directory which is used to incrementally
build the TAGS file and track modification dates in order to rebuild it quickly.

Use `tags-watcher.sh` to automatically rebuild the tags file when any haskell files are modified.
