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

