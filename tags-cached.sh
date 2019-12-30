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
    local cabalfiles=()
    local cabalprojroot=$(dirname "$1")
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

        if [[ "$PACKAGE_LIST" = true && ( ! -z "$LINE" ) ]]; then
            if [[ -d "$cabalprojroot/$LINE" ]]; then
                cabalfile="$cabalprojroot/$LINE/*.cabal"
                cabalfiles+=($cabalfile)
            else
                cabalfileglob="$cabalprojroot/$LINE"
                for i in $cabalfileglob
                do
                    if [[ -e "$i" ]]; then
                        cabalfiles+=("$i")
                    fi
                done
            fi
        fi
    done < "$1"

    for i in ${cabalfiles[@]}
    do
        local cabalroot=$(dirname "$i")
        while IFS= read -r dir; do
            CABALDIRS+=("$cabalroot/$dir")
        done < <(sed -n -e 's/^[[:space:]]*hs-source-dirs:[[:space:]]*\(.*\)/\1/p' "$i")
    done
}

if [[ "$TAGSUPDATED" = true || (! -e TAGS-global ) ]]; then
    echo "Updating collated TAGS file.."
    
    tmptags=$(mktemp)
    exec 3>"$tmptags"

    for srcdir in "${USRCDIRS[@]}"
    do
        cat "./TAGS-cache/$srcdir/TAGS" >> "$tmptags"
    done
    mv "$tmptags" TAGS-global
    echo "TAGS-global file updated."
    
    cabalprojs=()
    while IFS=  read -r -d $'\0'; do
       cabalprojs+=("$REPLY")
    done < <(find . -type f -name 'cabal.project' -not -path "*/dist-newstyle/*" -not -path "*/dist/*" -print0)

    for cabalproj in "${cabalprojs[@]}"
    do
        echo "Checking $cabalproj"
        updateneeded=false
        readcabaldirs "$cabalproj"
        cabalprojroot=$(dirname "$cabalproj")
        
        if [[ ! -e "$cabalprojroot/TAGS" ]]; then
            updateneeded=true
        else
            for srcdir in "${CABALDIRS[@]}"
            do
                cachedir="./TAGS-cache/$srcdir"
                echo "Checking cache dir: ./TAGS-cache/$srcdir"
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
                echo "Adding: $srcdir"
                cachedir="./TAGS-cache/$srcdir"
                find "$cachedir" -type f -name 'TAGS' -exec \
                     cat {} >> "$tmptags" \;
            done
            cabalprojroot2=${cabalprojroot:2}

            sed -e ':a' -e 'N' -e '$!ba' -e 's|''\n'$cabalprojroot2'/|''\
|g' "$tmptags" > "$cabalprojroot/TAGS"

        fi
    done
    find . -type f -name 'TAGS' | xargs touch
    
    echo "TAGS update finished."
    
else
    echo "No updates required."
fi
