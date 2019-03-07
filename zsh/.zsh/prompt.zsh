# %K/%k - start/stop background color
# %F/%f - start/stop foreground color
# %E    - clear to end of line - doesn't move cursor (ANSI Erase Line, \e[K)

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
#  - Set background color of whole line
#  - Remove syntax highlighting from entered command
#  - Add timestamp on right edge of screen
#
function _old_prompt() {
    # print: -n Do not add newline
    #        -r Do not process escape sequences
    #        -P Perform prompt expansion

    # We are currently on the line below the first line of the input command.

    # Turn off colors and erase anything on this line (ex RPROMPT)
    print -nP '%k%f%E'

    # Move back up to line with prompt
    tput cuu1

    # Set background color of entire line
    print -nP '%K{237}%E'

    # Re-print prompt
    print -nP "$PROMPT"

    # Make sure we still have our background color
    print -nP '%K{237}'

    # Re-print command
    print -nr "$1"

    # Figure out where we are
    print -n "\e[6n" # ESC [ Ps n (DSR)

    # Terminal response is ESC [ ROW ; COL R

    # read: -s  Turn off terminal echo
    #       -dR Input is terminated by R
    IFS=';' read -sdR CUR_ROW CUR_COL

    # Sometimes terminal response gets echoed anyway, clear it out
    tput hpa $((CUR_COL - 1))
    print -nP '%E'

    # Figure out where we want to print tag
    TAG="$(date '+[%Y-%m-%d %H:%M:%S]')"
    TAG_LEN=${#TAG}
    DST_COL=$(($(tput cols) - TAG_LEN))

    # If no room, add an extra line
    if [[ $DST_COL -lt $CUR_COL ]]; then
        print -nP '%E\n%E\n%K%E'
        tput cuu1
        print -nP '%K{237}'
    fi

    # Go to tag column
    tput hpa $DST_COL # Move cursor to column, ESC [ Ps G

    # Print tag
    print -nP '%F{240}'
    print -nr "$TAG"
    print -nP '%k%f'

    # Next character will appear at the beginning of the next row, but we're
    # actually still on the same line, so ex cursor positioning will not work
    # as expected, and window resizes might do weird things. So explicitly move
    # down to the next row, which is visually a no-op.
    tput cud1
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
