# syntax highlighting
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# prompt
source ~/.zsh/prompt.zsh

# vim mode
source ~/.zsh/vim.zsh

# misc
export EDITOR=vim

# fix ls
export QUOTING_STYLE=literal

# direnv
eval "$(direnv hook zsh)"

# more history
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE

# Save history from all terminals on execute
setopt INC_APPEND_HISTORY

# Add times to history
setopt EXTENDED_HISTORY

# Don't store commands starting with a space
setopt HIST_IGNORE_SPACE

# <@bigeasy> Shared history turns your shell history into a useless Twitter feed of what your alter egos are doing.
setopt NO_SHARE_HISTORY

# Don't clobber with redirection
setopt NO_CLOBBER
