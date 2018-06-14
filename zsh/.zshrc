# syntax highlighting
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# prompt
source ~/.zsh/prompt.zsh

# vim mode
source ~/.zsh/vim.zsh

# misc
export EDITOR=vim

# <@bigeasy> Shared history turns your shell history into a useless Twitter feed of what your alter egos are doing.
setopt NO_SHARE_HISTORY

# fix ls
export QUOTING_STYLE=literal

# direnv
eval "$(direnv hook zsh)"
