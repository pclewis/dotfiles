#!/bin/sh
export PATH=$PATH:$HOME/.nix-profile/bin
while read -a event
do
    dir=$HOME/.config/kooh/"$event"
    dirs=()
    while true; do
        echo Checking $dir
        if [[ -d "$dir" ]]; then
            echo Found $dir
            dirs+=("$dir")
        fi
        if [[ "$dir" == $HOME/.config/kooh ]]; then
            break
        else
            dir=${dir%/*}
        fi
    done

    find "$dirs" -maxdepth 1 -type f -executable -uid $(id -u) -not -perm /go+w -exec {} "${event[@]}" \;
done
