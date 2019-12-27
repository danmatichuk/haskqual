#!/bin/bash


while true; do
    find . -type f \( -iname \*.hs -o -iname \*.x -o -iname \*.lhs \) -not -path "*/dist-newstyle/*" -not -path "*/dist/*" | entr -d tags-cached.sh
    if [[ $? -gt 128 ]]; then
        break
    else
        sleep 2
   fi
done

