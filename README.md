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

## Emacs ##

After loading `haskqual.el`, qualified haskell lookups can be performed by executing `haskqual-find-definition` with the point on an identifier you wish to lookup. This will perform several calls to the `etags` backend of xrefs with different possibilities for fully-qualified variants of the given identifier. The the results of the first successful call will then be shown.

### Example: ###

Given the following module structure:

```haskell
module Bar where

barInt :: Integer
barInt = 0
```

```haskell
module Buzz where

buzzInt :: Integer
buzzInt = 1
```
```haskell
module Quirk where

import qualified Bar as B
import           Buzz
import qualified Buzz as B

quirkInt :: Integer
quirkInt = B.barInt + buzzInt
```

A query for `B.barInt` will be expanded into both `Bar.buzzInt` and `Buzz.barInt`.
A query for `buzzInt` will be expanded into `Quirk.buzzInt` and `Buzz.buzzInt`.

Each of these will only have one successful resolution.



