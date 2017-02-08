# syntax highlighting
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# prompt
source ~/.zsh/prompt.zsh

setopt PROMPT_SUBST

RPROMPT=""
PROMPT=$'$(_colorize $USER)%F{8}@%f$(_colorize $HOST)%F{8}:%f$(_prompt_pwd)%F{8}%#%f '

# misc
export EDITOR=vim

# <@bigeasy> Shared history turns your shell history into a useless Twitter feed of what your alter egos are doing.
setopt NO_SHARE_HISTORY

# fix ls
export QUOTING_STYLE=literal
