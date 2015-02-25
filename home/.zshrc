# early, fast invocation of tmux
# - only if tmux is installed
# - not in linux ttys
# - no nested tmux sessions
if [[ -n ${commands[tmux]} && "$TERM" != "linux" && -z "$TMUX" ]]; then
  tmux attach-session || tmux
  [[ $? = "0" ]] && exit
fi

# Predictable ssh auth sockets
SOCK="${HOME}/.ssh/ssh_auth_sock"
if test $SSH_AUTH_SOCK && [ $SSH_AUTH_SOCK != $SOCK ]
then
  ln -sf $SSH_AUTH_SOCK $SOCK
  export SSH_AUTH_SOCK=$SOCK
fi

source $HOME/.zshuery/zshuery.sh
load_defaults
load_aliases
load_completion $HOME/.zsh-completion/src

for file in ~/.zsh/*[^~]; do
  autoload -U "${file##*/}"
done

bindkey -e
source "$HOME/.zprompt"
source "$HOME/.zaliases"

# jobs
setopt long_list_jobs
zstyle ':completion:*' menu select

[ -f $HOME/.zprofile ] && source $HOME/.zprofile
[ -f $HOME/.zshrc.$HOST ] && source $HOME/.zshrc.$HOST

if [ -f "$HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]; then
  source "$HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi

if [ -f "$HOME/.zsh-history-substring-search/zsh-history-substring-search.zsh" ]; then
  source "$HOME/.zsh-history-substring-search/zsh-history-substring-search.zsh"
  # bind P and N for EMACS mode
  bindkey -M emacs '^P' history-substring-search-up
  bindkey -M emacs '^N' history-substring-search-down
fi

if [ -f "$HOME/.zsh-autosuggestions/autosuggestions.zsh" ]; then
  source "$HOME/.zsh-autosuggestions/autosuggestions.zsh"
  # Enable autosuggestions automatically
  zle-line-init() { zle autosuggest-start }
  zle -N zle-line-init

  export AUTOSUGGESTION_HIGHLIGHT_COLOR='fg=10'
fi

if [ -f $HOME/.homesick/repos/homeshick/homeshick.sh ]; then
  source $HOME/.homesick/repos/homeshick/homeshick.sh
fi

fpath=($ZDOTDIR/functions/*(/N) $fpath)
# autoload all public functions
for file in $ZDOTDIR/functions/*/*(.N:t); do
  autoload -U $file
done
unset file

# {{{ Functions
flash_undelete() {
  cd /proc/$(ps x | awk '/libflashplayer.so\ /{print $1}')/fd && ls -l | grep deleted
}

webserver() {
  # dirty hack to get the network interfaces
  # and its local ip address
  ip addr show | grep --color=never --perl-regexp '^\d:|inet'

  [[ -n commands[avahi-publish-service] ]] && avahi-publish-service "Joergs Webserver" _http._tcp 8080 &
  trap "kill $!" SIGINT SIGTERM EXIT
  python -m http.server 8080
}

pull-dotfiles() {
  pushd ~/.homesick/repos/dotfiles
  git submodule foreach --recursive "git fetch --all; git reset --hard origin/master"
  popd
}

# Autoinstall Bundle
bundle() {
  if [ -z "${commands[bundle]}" ] && [ -n "${commands[gem]}" ]; then
   gem install --user-install bundler
  fi
  command bundle "$@"
}

ff() { /usr/bin/find . -iname "*$@*" }

function {emacs,ee}merge() {
  if [ $# -ne 2 ]; then
    echo Usage: $0 local base other
    return 1
  fi
  local in_shell
  if [[ "$0" == "eemerge" ]]; then
    in_shell="-nw"
  fi
  emacs $in_shell --eval '(ediff-merge "'$1'" "'$2'")'
}

browse () { $BROWSER file://"`pwd`/$1" }

function retry() {
  local n=0
  local trys=${TRYS:-100000}
  local sleep_time=${SLEEP:-1}
  until ($1 "${@:2}") ; do
      n=$(( n + 1 ))
      [ $n -gt $trys ] && return 1
      sleep $sleep_time
  done
}

function own() {
  if [ -n "${commands[sudo]}" ]; then
    sudo chown -R "$USER:$(id -gn)" "$@"
  else
    chown -R "$USER:$(id -gn)" "$@"
  fi
}
# }}}
#

# starting with gnupg 2.1.0, ssh-agent uses static socket paths, which make
# invocation much easier
if [[ -z "$SSH_CLIENT" ]]; then
  export GPG_AGENT_INFO=$HOME/.gnupg/S.gpg-agent
  export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
  if [[ -n "${commands[gpg-agent]}" && ! -S "$HOME/.gnupg/S.gpg-agent.ssh" ]]; then
    eval "$(gpg-agent --daemon)"
    [ -f ~/.ssh/id_rsa.pub ] || [ -f ~/.ssh/id_ecdsa ] && ssh-add
  fi
fi

if [ -n "${commands[direnv]}" ]; then
  eval "$(direnv hook zsh)"
fi

setopt autopushd

# Cycle directory with Ctrl-Right and Ctrl-Left
eval "insert-cycledleft () { zle push-line; LBUFFER='pushd -q +1'; zle accept-line }"
zle -N insert-cycledleft
bindkey "^[[1;5C" insert-cycledleft
eval "insert-cycledright () { zle push-line; LBUFFER='pushd -q -0'; zle accept-line }"
zle -N insert-cycledright
bindkey "^[[1;5D" insert-cycledright

# Terminal stuff
ulimit -S -c 0 # disable core dumps
stty -ctlecho # turn off control character echoing

if [[ $TERM = linux ]]; then
  setterm -regtabs 2 # set tab width of 4 (only works on TTY)
fi

chpwd() {
  update_terminal_cwd

  # List directory after changing directory
  ls
}

# }}}

# usage
# vil file:20 -> opens file on line 20
vil() {
  setopt shwordsplit
  IFS=':' ARGS=($@)
  unsetopt shwordsplit
  vim +${ARGS[2]} ${ARGS[1]}
}

# OPAM configuration
[ -f "$HOME/.opam/opam-init/init.zsh" ] && . "${HOME}/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null

if [ -f /usr/share/chruby/chruby.sh ]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh
elif [ -d "$HOME/.rvm/bin" ]; then
  export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
fi
