#ANTIGEN
source $HOME/.zsh/antigen/antigen.zsh

antigen bundles <<EOF
  git
  lein
  zsh-users/zsh-syntax-highlighting
EOF

POWERLEVEL9K_PROMPT_ON_NEWLINE=true

antigen theme bhilburn/powerlevel9k powerlevel9k

antigen apply

# misc
export EDITOR=vim

# <@bigeasy> Shared history turns your shell history into a useless Twitter feed of what your alter egos are doing.
# TODO: murder whoever is setting this in the first place
setopt NO_SHARE_HISTORY
