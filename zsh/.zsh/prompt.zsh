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


## _old_prompt(input,cmd,fullcmd)
#
#  Make prompts look different when they're history
#
function _old_prompt() {
    print -nP '%k%f%E'
    tput cuu1
    print -nP '%K{237}%E\r'
    COL=$(($(tput cols) - 22))
    print -nP "%K{237}$PROMPT%K{237}"
    print -nR $1
    print -nP "\e[${COL}G%F{240}" $(date '+[%Y-%m-%d %H:%M:%S]') '%k%f'
}

add-zsh-hook preexec _old_prompt

# expand things in prompt
setopt PROMPT_SUBST

# When output doesn't end with newline, add a % and newline instead of
# clobbering it with the prompt
setopt PROMPT_SP

# here it is, my beautiful prompt
RPROMPT=""
PROMPT=$'$(_colorize $USER)%F{8}@%f$(_colorize $HOST)%F{8}:%f$(_prompt_pwd)%F{8}%#%f %K{231}'
