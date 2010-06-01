# If not running interactively, don't do anything
[ -z "$PS1" ] && return

. ~/dotfiles/bashrc.prexec
. ~/dotfiles/bashrc.colors
. ~/dotfiles/bashrc.ssh-agent
. ~/dotfiles/bashrc.prompt

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi

# some more ls aliases
alias ll='ls -l'

alias dc=cd

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

#export CSCOPE_DB=

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

export HISTSIZE=50000
#export HISTTIMEFORMAT="%m-%d %H:%M"

shopt -s autocd    # executing a dir cds into it
shopt -s globstar  # enable **
shopt -s dirspell  # fix dir spelling in tab completion

#export PROMPT_DIRTRIM=5


# recursive grep by extension
# ex: f c -i 'hello world'
f () {
  ext=$1
  shift
  find . -path "*.$ext" -type f -exec grep -Hn "$@" {} \;
}

# find file by name
# fn [path] file
# ex: fn src *.java
fn () {
  path=./
  if [ $# -gt 1 ]; then
    path="$1"
    shift
  fi

  find "$path" -iname "$@"
}

# grails crap
export PATH=$PATH:~/grails/bin
export GWT_HOME=~/Downloads/gwt-linux-1.7.1

# ccache
if [ -d /usr/lib/ccache/bin ]
then export PATH=/usr/lib/ccache/bin:$PATH
fi

export FIGNORE=.svn:.git

source /etc/profile.d/bash-completion.sh
