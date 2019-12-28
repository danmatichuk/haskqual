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

CABALDIRS=()
function readcabaldirs () {
    local PACKAGE_LIST=false
    CABALDIRS=()
    local cabalroot=$(dirname "$1")
    while IFS= read -r p; do
        local LINE="$p"
        if [[ ( "$p" =~ ^"optional-packages:"[[:space:]]*(.*) ) || ( "$p" =~ ^"packages:"[[:space:]]*(.*) )  ]]; then
            PACKAGE_LIST=true
            LINE="${BASH_REMATCH[1]}"
        elif [[ "$p" =~ ^[[:space:]]+([^[:space:]]+) ]]; then
            LINE="${BASH_REMATCH[1]}"
        else
            PACKAGE_LIST=false
        fi

        if [[ "$PACKAGE_LIST" = true && ( ! -z "$LINE" ) && -d "$cabalroot/$LINE" ]]; then
            CABALDIRS+=("$cabalroot/$LINE")
        fi
    done < "$1"
}

if [[ "$TAGSUPDATED" = true || (! -e TAGS-global ) ]]; then
    echo "Updating collated TAGS file.."
    
    echo > "./TAGS-new"
    for srcdir in "${USRCDIRS[@]}"
    do
        cat "./TAGS-cache/$srcdir/TAGS" >> TAGS-new
    done
    mv TAGS-new TAGS-global
    echo "TAGS-global file updated."
    
    cabalprojs=()
    while IFS=  read -r -d $'\0'; do
       cabalprojs+=("$REPLY")
    done < <(find . -type f -name 'cabal.project' -not -path "*/dist-newstyle/*" -not -path "*/dist/*" -print0)

    for cabalproj in "${cabalprojs[@]}"
    do
        updateneeded=false
        readcabaldirs "$cabalproj"
        cabalprojroot=$(dirname "$cabalproj")
        
        if [[ ! -e "$cabalprojroot/TAGS" ]]; then
            updateneeded=true
        else
            for srcdir in "${CABALDIRS[@]}"
            do
                cachedir="./TAGS-cache/$srcdir"
                if [[ -e "$cachedir" && $(find "$cachedir" -name 'TAGS' -newer "$cabalprojroot/TAGS") ]]; then
                    updateneeded=true
                    echo "Found updated TAGS for $cachedir and $cabalprojroot/TAGS"
                fi
            done
        fi
        
        if [[ $updateneeded = true ]]; then
            echo "Updating TAGS for cabal project at: $cabalproj"
            
            tmptags=$(mktemp)
            exec 3>"$tmptags"
            
            for srcdir in "${CABALDIRS[@]}"
            do
                cachedir="./TAGS-cache/$srcdir"
                find "$cachedir" -type f -name 'TAGS' -exec \
                     cat {} >> "$tmptags" \;
            done
                
            mv "$tmptags" "$cabalprojroot/TAGS"
        fi
    done
    find . -type f -name 'TAGS' | xargs touch
    
    echo "TAGS update finished."
    
else
    echo "No updates required."
fi
