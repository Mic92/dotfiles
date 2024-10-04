#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export TTY=$(tty)

# shell opts: see bash(1)
shopt -s autocd cdspell dirspell extglob no_empty_cmd_completion
shopt -s checkwinsize checkhash
shopt -s histverify histappend histreedit cmdhist

set -o notify    # notify of completed background jobs immediately
ulimit -S -c 0   # disable core dumps
stty -ctlecho    # turn off control character echoing

if [[ $TERM == linux ]]; then
  setterm -regtabs 2 # set tab width of 4 (only works on TTY)
fi

# more for less
export LESS=-R # use -X to avoid sending terminal initialization
export LESS_TERMCAP_mb=$'\e[01;31m'
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m' # more for less

# history
export HISTIGNORE="&:ls:[bf]g:exit:reset:clear:cd*"
export HISTCONTROL="ignoreboth:erasedups"
export HISTSIZE=1000
export HISTFILESIZE=2000

source_bash_completion() {
  local f
  [[ $BASH_COMPLETION ]] && return 0
  for f in /{etc,usr/share/bash-completion}/bash_completion; do
    if [[ -r $f ]]; then
      . "$f"
      return 0
    fi
  done
}

function ff() { find . -type f -iname '*'"$*"'*' -ls; }

# External config
if [[ -r ~/.dircolors ]] && type -p dircolors >/dev/null; then
  eval $(dircolors -b "$HOME/.dircolors")
fi

source_bash_completion
unset -f source_bash_completion

source "$HOME/.homesick/repos/homeshick/homeshick.sh"

PS1='[\[\033[1;36m\]\u\[\033[0m\]@\h \W]\$ '

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias grep='grep --color'
alias info='info --vi-keys'
alias j='jobs'
alias ll='ls -l --color'
alias lla='ls -la --color'
alias ls='ls --group-directories-first --color'
alias sudo='sudo '
alias webshare='python -m http.server 8080'
alias wgetxc='wget $(xclip -o)'
alias grep="grep --binary-files=without-match --directories=skip --color=auto"

if [[ -r ~/.bashrc."$HOSTNAME" ]]; then
  . ~/.bashrc."$HOSTNAME"
fi

if command -v direnv &>/dev/null; then
  eval "$(direnv hook bash)"
fi
