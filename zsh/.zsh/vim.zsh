# Activate vim keys
bindkey -v

# make esc switch faster
export KEYTIMEOUT=1

# change color 231 based on mode
function zle-line-init zle-keymap-select {
  case $KEYMAP in
      main) printf "\e[6 q\e]4;231;#292b2e\e\\" ;; # #313f28
      vicmd) printf "\e[2 q\e]4;231;#423B2A\e\\" ;;
  esac
  print -nP "\r%K{231}%E"
  zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

# backspace over everything
bindkey "^h" backward-delete-char
bindkey "^?" backward-delete-char

# backwards search history
bindkey "^r" history-incremental-search-backward

# Make / and ? search current line instead of history
source ~/.zsh/zsh-vi-search/src/zsh-vi-search.zsh
