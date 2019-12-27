#!/bin/bash
FASTTAGSCMD="fast-tags --emacs --fully-qualified"

echo "Checking tags cache for updates.."

array=()
while IFS=  read -r -d $'\0'; do
    array+=("$REPLY")
done < <(find . -type f \( -iname \*.hs -o -iname \*.x -o -iname \*.lhs \) -not -path "*/dist-newstyle/*" -not -path "*/dist/*" -print0)

SRCDIRS=()
for src in "${array[@]}"
do
    srcdir=$(dirname $src)
    SRCDIRS+=("$srcdir")
done

USRCDIRS=($(printf "%s\n" "${SRCDIRS[@]}" | sort -u))
TAGSUPDATED=false

for srcdir in "${USRCDIRS[@]}"
do
    mkdir -p "./TAGS-cache/$srcdir"
    TAGFILE="./TAGS-cache/$srcdir/TAGS"
    if [[ (! -e "$TAGFILE" ) || $(find "$srcdir" -maxdepth 0 -type d -newer "$TAGFILE") ]]; then
        CMD="find "$srcdir" -maxdepth 1 -type f \( -iname \*.hs -o -iname \*.x -o -iname \*.lhs \) | xargs $FASTTAGSCMD -o \"$TAGFILE\""
        echo "Updating tags cache for $srcdir"
        touch "$TAGFILE"
        eval "$CMD"
        TAGSUPDATED=true
    fi
done

if [[ "$TAGSUPDATED" = true || (! -e TAGS ) ]]; then
    echo "Updating collated TAGS file.."
    rm -f TAGS
    for srcdir in "${USRCDIRS[@]}"
    do
        cat "./TAGS-cache/$srcdir/TAGS" >> TAGS
    done
    echo "TAGS file updated."
else
    echo "No updates required."
fi
