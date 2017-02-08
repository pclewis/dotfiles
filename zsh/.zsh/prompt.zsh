## _prompt_pwd():
#
#  Fancy colorized prompt PWD.
#
#  Shortens intermediate directories into their shortest unique prefix.
#
#  Directories that contain a file called '.dont-abbrev' or have more
#  than ~100 siblings will not be shortened.
#
function _prompt_pwd() {
    if [[ "$PWD" == "$HOME" ]]; then
        echo "~"
        return
    fi

    d=$PWD
    s=$d:t
    d=$d:h
    while true; do 
        if [[ "$d" == "$HOME" ]]; then
            s="~/"$s
            break;
        elif [[ "$d" == "/" ]]; then
            s="/"$s
            break;
        fi

        t=$d:t
        h=$d:h
        n_links=$(stat -c%h $h)

        if [[ $n_links -gt 100 ]]; then
            s='%F{9}'$t'%f'/$s
        elif [[ -e $h/.dont-abbrev ]]; then
            s='%F{10}'$t'%f'/$s
        else
            nc=1
            p=""
            while [[ "$p" != "$t" ]]; do
                p=${t[1,$nc]}
                pf=$(echo $h/$p*)
                if [[ "${(w)#pf}" -eq 1 ]]; then
                    break
                fi
                (( nc = nc + 1 ))
            done
            s='%F{5}'$p'%f'/$s
        fi
        d=$h
    done
    echo $s
}

## _colorize(str):
# 
#  Return str with a random color based on its content.
#
function _colorize() {
    n=$(( $( echo $1 | cksum | cut -f1 -d' ') % 231 ))
    #echo $'\e[38;5;'$n'm'$1'\e[0m'
    echo '%F{'$n'}'$1'%f'
}
