#!/bin/zsh

LN="ln"

while getopts "f" ARG; do
    case "$ARG" in
        f)
            LN="ln -f" ;;
        ?)
            ;;
    esac
done
shift $[$OPTIND-1]


for f in emacs.d abbrev_defs; do
    OUTPUT=$($=LN -svT $f:A $HOME/.$f 2>&1)
    if [ "$?" = "0" ]; then
        echo "[32;1m$OUTPUT[0m" 1>&2
    else
        echo "[31;1m$OUTPUT[0m" 1>&2
    fi
done

if [ ! -d $HOME/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python # sorry, future me
fi
