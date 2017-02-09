#!/bin/sh
f="$XDG_RUNTIME_DIR"/kooh/kooh
if [[ -p $f ]]; then
    echo "$@" >> $f
fi
