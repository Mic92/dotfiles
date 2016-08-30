# early, fast invocation of tmux
# - only if tmux is installed
# - not in linux ttys
# - no nested tmux sessions

if [[ -n ${commands[tmux]} && "$TERM" != "linux" && -z "$TMUX" ]]; then
  tmux attach-session || tmux
  [[ $? = "0" ]] && exit
fi

hash_string256() {
  local hashval
  hashval=$(printf "%s" "$1" | md5sum)
  # upcase & substring
  hashval="0x${(L)hashval[1,15]}"
  (( y = (hashval + 110) % 255 ))
  printf "%d" $y
}
if [[ "$__host__" != "$HOST" ]]; then
  tmux set -g status-bg colour$(hash_string256 $HOST)
  __host__=$HOST
fi

##  Helpers
# xalias - only supposed to be used with simple aliases.
xalias() {
  local key val com
  if (( ${#argv} == 0 )) ; then
    printf 'xalias(): Missing argument.\n'
    return 1
  fi
  if (( ${#argv} > 1 )) ; then
    printf 'xalias(): Too many arguments %s\n' "${#argv}"
    return 1
  fi

  key="${1%%\=*}" ;  val="${1#*\=}"
  check_com ${val} && alias -- "${key}=${val}"
  return 0
}
# xhashd - check for directory, then create hash -d
xhashd() {
  local key val com
  if (( ${#argv} == 0 )) ; then
    printf 'xhashd(): Missing argument.\n'
    return 1
  fi
  if (( ${#argv} > 1 )) ; then
    printf 'xhashd(): Too many arguments %s\n' "${#argv}"
    return 1
  fi

  key="${1%%\=*}";  val="${1#*\=}"
  [[ -d ${val} ]] && hash -d -- "${key}=${val}"
  return 0
}
# check_com - check if a command exists
# eg: check_com "vim -p"
function check_com() {
    local -a words
    local -i comonly
    local cmd

    if [[ ${1} == '-c' ]] ; then
        (( comonly = 1 ))
        shift
    else
        (( comonly = 0 ))
    fi

    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c] <command>\n' >&2
        return 1
    fi

    words=(${(z)1})
    cmd=${words[1]}

    if (( comonly > 0 )) ; then
        [[ -n ${commands[$cmd]}  ]] && return 0
        return 1
    fi

    if   [[ -n ${commands[$cmd]}    ]] \
      || [[ -n ${functions[$cmd]}   ]] \
      || [[ -n ${aliases[$cmd]}     ]] \
      || [[ -n ${reswords[(r)$cmd]} ]] ; then
        return 0
    fi
    return 1
}
is_mac() { [[ $OSTYPE == darwin* ]] }
is_freebsd() { [[ $OSTYPE == freebsd* ]] }
is_linux() { [[ $OSTYPE == linux-gnu ]] }
upfind() {
  local previous=
  local current=$PWD

  if [[ $# -ne 1 ]];then
    echo "$0 FILE_NAME"
    return 1
  fi

  while [[ -d $current && $current != $previous ]]; do
    local target_path=$current/$1
    if [[ -f $target_path ]]; then
      echo $target_path
      return 0
    else
      previous=$current
      current=$current:h
    fi
  done
  return 1
}
clone(){
  [ $# -eq 0 ] && echo "clone <GIT_CLONE_URL>" && return 1

  cd `mktemp -d`
  git clone --depth=1 "$1"
}

## Options
setopt auto_name_dirs
setopt pushd_ignore_dups
setopt prompt_subst
setopt no_beep
setopt auto_cd
setopt correct
setopt multios
setopt cdablevarS
setopt transient_rprompt
setopt extended_glob
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
autoload -U zmv
bindkey "^[m" copy-prev-shell-word
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
DIRSTACKSIZE=30
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>' # Like default, but without / -- ^W must be useful in paths, like it is in vim, bash, tcsh
setopt hist_ignore_dups
setopt hist_reduce_blanks
setopt share_history
setopt append_history
setopt hist_verify
setopt inc_append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_space
setopt long_list_jobs
# emacs keys
bindkey -e

## Completion
autoload colors; colors;
autoload -U compinit
fpath=($* "/usr/local/share/zsh/site-functions" $fpath)
fignore=(.DS_Store $fignore)
compinit -i
compdef mcd=cd
zmodload -i zsh/complist
setopt complete_in_word
unsetopt always_to_end
[[ -f ~/.ssh/known_hosts ]] && hosts=(`awk '{print $1}' ~/.ssh/known_hosts | tr ',' '\n' `)
[[ -f ~/.ssh/config ]] && hosts=($hosts `grep '^Host' ~/.ssh/config | sed s/Host\ // | egrep -v '^\*$'`)
zstyle ':completion:*' insert-tab pending
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
highlights='${PREFIX:+=(#bi)($PREFIX:t)(?)*==31=1;32}':${(s.:.)LS_COLORS}}
highlights2='=(#bi) #([0-9]#) #([^ ]#) #([^ ]#) ##*($PREFIX)*==1;31=1;35=1;33=1;32=}'
zstyle -e ':completion:*' list-colors 'if [[ $words[1] != kill && $words[1] != strace ]]; then reply=( "'$highlights'" ); else reply=( "'$highlights2'" ); fi'
unset highlights
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:hosts' hosts $hosts
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ./cache/
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"
[ -d "$HOME/.zsh-completions/src" ] && fpath=($HOME/.zsh-completions $fpath)

## Prompt
source "$HOME/.zprompt"

## Aliases
# Basic commands
alias zcat='zcat -f'
# - same as tail but using inotify for better perfomance
# - linux only
xalias tail='inotail'
xalias ag='ag --color --smart-case --literal'
xalias rag='ag --color --smart-case --path-to-agignore ~/.ruby-agignore'
# System tools
xalias top='htop'
alias free='free -m'
alias vuser="fuser -v "
alias du='du -hc'
alias df='df -hT'
xalias df='dfc'
# File management
if [[ $OSTYPE == freebsd* ]]; then
  alias ls='ls -G'
else
  alias ls='ls --color=auto --classify --human-readable --group-directories-first'
fi
alias sl=ls
alias la='ls -lA'
alias laa='ls -la'
alias ll='ls -l'
alias tempdir='cd `mktemp -d`;'
alias rm='rm -rv'
alias cp='nocorrect cp -rpv'
alias cpv="rsync -pogr --progress"
alias ln="nocorrect ln"
alias mv='nocorrect mv -v'
alias mkdir='nocorrect mkdir -p'
xalias locate='locate --existing --follow --basename --ignore-case'
alias ip='ip -c'
# noglobs
alias wget='noglob wget'
alias curl='noglob curl'
alias git='noglob git'
alias rake='noglob rake'
# Root
# fallback if sudo is not yet installed
alias su='su - '
alias sync='sudo sync'
alias updatedb='sudo updatedb'
alias sless='sudo less'
alias stail='sudo tail'
xalias ctl='sudo systemctl'
alias json_escape="ruby -e \"require 'json'; puts(File.open(ARGV[0]).read.to_json) if ARGV[0]\""
alias gdb='gdb --quiet'
# Editors
[[ -n ${commands[vi]} ]] && alias vi=vim
xalias vim="nvim"
xalias ee="emacs -nw"
# Package management
if [[ -f /etc/debian_version ]] ; then
  alias apt-get='sudo apt-get'
  alias apt-secure='sudo apt-secure'
  alias apt-file='sudo apt-file'
  alias aptitude='sudo aptitude'
  alias dpkg='sudo dpkg'
elif [[ -f /etc/arch-release ]] ; then
  # if colored version is avaible
  xalias pacman='pacman-color'
  xalias y=yaourt
  # run as root
  alias pacman='sudo pacman'
  alias abs='sudo abs'
  # pacman-contrib stuff
  xalias pacdiff='sudo pacdiff -l'
  xalias pacscripts='sudo pacscripts'
  xalias pactree='pactree -c'
  xhashd yaourtbuild=/var/abs/local/yaourtbuild
  function owns() { /usr/bin/pacman -Qo $(which $1)}
  alias pacunlock="sudo rm /var/lib/pacman/db.lck"
elif [[ -f /etc/gentoo-release ]] ; then
  alias emerge='sudo emerge'
  xalias eix-sync='sudo eix-sync -C --quiet'
fi
# Dir Hashes
xhashd awesome=~/.config/awesome/
xhashd vicious=~/.config/awesome/vicious
xhashd mic92=~/go/src/github.com/Mic92
xhashd git=~/git
# Global aliases
alias -g G='| grep -'
alias -g L='| less'
alias -g C='| xclip'
alias -g H='| head'
alias -g N='&>/dev/null'
alias -g SL='| sort | less'
alias -g S='| sort -u'
alias -g T='| tail'
alias -g W='| wc -l'
# Miscellanious
# a fresh zsh, please.
alias fzsh='PS1="zsh%# " zsh -f'
# generic aliases
# diff format like git
xalias diff='diff -Naur --strip-trailing-cr'
[ -z "${commands[ping6]}" ] && ping6="ping -6"
alias gping="ping google.com"
alias gping6="ping6 google.com"
alias ghost="host -v google.com 8.8.8.8"
alias gcurl="curl -v google.com"
ping_gw(){ ip route | awk '/default/ {printf "-I %s %s", $5, $3; exit}' | xargs --no-run-if-empty --max-args 3 ping -W2 }
ping6_gw(){ ip -6 route | awk '/default/ {printf "-I %s %s", $5, $3; exit}' | xargs --no-run-if-empty --max-args 3 ping6 -W2 }
alias :q=exit
alias todotxt="vim ~/Dropbox/todo/todo.txt"
alias grep="grep --binary-files=without-match --directories=skip --color=auto"

## PROFILE
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/.cargo/bin
    $HOME/go/bin
    $HOME/.go/bin
    $HOME/.gem/ruby/*.*.*/bin(NOn[1])
    $path
)
fpath=(~/.zsh $fpath)
# get rid of duplicate
typeset -U path
typeset -U fpath
# remove non-existing entries from path
path=($^path(N))
export PATH
cdpath=( ~/git )
# Prefered programs
export BROWSER=chromium
export TERMINAL=urxvt
export PICTUREVIEW=eog
export EDITOR=vim
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=vim
export PAGER=less
export ACK_PAGER=$PAGER
export READNULLCMD=$PAGER
export pacman_program=pacman-color
# X11, Sound, Graphic
export XDG_CACHE_HOME=~/.cache
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.data
export ERRFILE=~/.xsession-errors
# Antialising
export QT_XFT=1
export GDK_USE_XFT=1
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# To enable Graphic Hardware acceleration
#export LIBGL_ALWAYS_INDIRECT=1
export INTEL_BATCH=1
# Enable Pulse for SDL
export SDL_AUDIODRIVER=pulse
# fix broken xdg-open
export GDMSESSION=1 GNOME_DESKTOP_SESSION_ID=1
# make OpenJDK working with awesome wm
export JAVA_FONTS=/usr/share/fonts/TTF
# less
export LESS=-FXisRM
export LESS_TERMCAP_mb=$'\E[01;31m'     # begin blinking
export LESS_TERMCAP_me=$'\E[0m'         # end mode
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'  # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'         # end underline
export LESS_TERMCAP_us=$'\E[03;33;146m' # begin underline is now yellow, italic
#                           |  |  |
#                           |  |----------------- yellow
#                           |-------------------- italic
# Man
export MANWIDTH=80
# If the execution of a command takes longer than
# REPORTTIME (in seconds),  time statistics are printed
export REPORTTIME=4
# Enforce correct locales from the beginning:
# LC_ALL is unset since it overwrites everything
# LANG=de_DE.UTF-8 is used, except for:
# LC_MESSAGES=en_DK.UTF-8 never translates program output
# LC_TIME=en_DK.UTF-8 leads to yyyy-mm-dd hh:mm date/time output
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=en_DK.UTF-8
export LC_TIME=en_DK.UTF-8
export PERL_CPANM_OPT="--local-lib=~/.perl5"
export PERL5LIB=~/.perl5/lib/perl5
export PYTHONDOCS=/usr/share/doc/python/html/
if [ -x "$HOME/.go/bin/go" ]; then
  export GOROOT="$HOME/.go"
  export GOROOT_BOOTSTRAP=/usr/lib/go
fi
export GOPATH="$HOME/go"
[ ! -d "$GOPATH" ] && mkdir -p "$GOPATH/src" 2>/dev/null
export NIX_PATH="$HOME/.nix"

## Functions
flash_undelete() {
  cd /proc/$(ps x | awk '/libflashplayer.so\ /{print $1}')/fd && ls -l | grep deleted
}
network() {
  (
    echo "$fg_bold[green]# Interfaces$reset_color"
    ip a s $1 || ip a s | grep "$1"
    (
      echo "\n$fg_bold[green]# Routes (v4) $reset_color"
      ip route;
      echo "\n$fg_bold[green]# Routes (v6) $reset_color"
      ip -6 route;
      echo "\n$fg_bold[green]# Rule $reset_color"
      ip rule;
      echo "\n$fg_bold[green]# Neighbor $reset_color"
      ip neigh
    ) | grep --color=always "$1"
  ) | less '+/#'
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
ff() { /usr/bin/find . -iname "*$@*" 2>/dev/null }
browse () { $BROWSER file://"`pwd`/$1" }
retry() {
  local n=0
  local trys=${TRYS:-100000}
  local sleep_time=${SLEEP:-1}
  until ($1 "${@:2}") ; do
      n=$(( n + 1 ))
      [ $n -gt $trys ] && return 1
      sleep $sleep_time
  done
}
own() {
  if [ -n "${commands[sudo]}" ]; then
    sudo chown -R "$USER:$(id -gn)" "$@"
  else
    chown -R "$USER:$(id -gn)" "$@"
  fi
}
cdf(){
  cd "$(dirname $1)"
}
# usage
# vil file:20 -> opens file on line 20
vil() {
  setopt shwordsplit
  IFS=':' ARGS=($@)
  unsetopt shwordsplit
  vim +${ARGS[2]} ${ARGS[1]}
}
jtes() {
  curl jtes.halfco.de/sets/1.json | jq "map({id: .id, created_at: .created_at, desc: .summary ,url: .track.permalink_url })" | less
}
# force output to be on a single line
ss() {
  # -p requires sudo to see all processes
  if echo "$@" | grep -q "p"; then
    sudo ss "$@" | tee
  else
    command ss "$@" | tee
  fi
}
# Autossh - try to connect every 0.5 secs (modulo timeouts)
sssh(){ while true; do command ssh -q "$@"; [ $? -ne 0 ] && break || sleep 0.5; done }
moshlogin(){ ssh login killall mosh-server; mosh -A -p 60011:60011 login }
# List directory after changing directory
chpwd() { ls }
mcd() { mkdir -p "$1" && cd "$1"; }
pj() { python -mjson.tool } # pretty-print JSON
cj() { curl -sS $@ | pj } # curl JSON
md5() { echo -n $1 | openssl md5 /dev/stdin }
sha1() { echo -n $1 | openssl sha1 /dev/stdin }
sha256() { echo -n $1 | openssl dgst -sha256 /dev/stdin }
sha512() { echo -n $1 | openssl dgst -sha512 /dev/stdin }
rot13() { echo $1 | tr "A-Za-z" "N-ZA-Mn-za-m" }
urlencode() { python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
urldecode() { python2 -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
last_modified() { ls -t $* 2> /dev/null | head -n 1 }
cheat() { command cheat "$@" | less }
ninja(){
  local build_path
  build_path=$(dirname "$(upfind "build.ninja")")
  command ninja -C "${build_path:-.}" "$@"
}
make(){
  local build_path
  build_path=$(dirname "$(upfind "Makefile")")
  command make -C "${build_path:-.}" "$@"
}

## Autocycle
setopt autopushd
# Cycle directory with Ctrl-Right and Ctrl-Left
eval "insert-cycledleft () { zle push-line; LBUFFER='pushd -q +1'; zle accept-line }"
zle -N insert-cycledleft
bindkey "^[[1;5C" insert-cycledleft
eval "insert-cycledright () { zle push-line; LBUFFER='pushd -q -0'; zle accept-line }"
zle -N insert-cycledright
bindkey "^[[1;5D" insert-cycledright

## Terminal stuff
ulimit -S -c 0 # disable core dumps
stty -ctlecho # turn off control character echoing
if [[ $TERM = linux ]]; then
  setterm -regtabs 2 # set tab width of 4 (only works on TTY)
fi

## Per machine zshrc
[ -f $HOME/.zshrc.$HOST ] && source $HOME/.zshrc.$HOST

# added by travis gem
[ -f /home/joerg/.travis/travis.sh ] && source /home/joerg/.travis/travis.sh

## Plugins
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
if [ -f "$HOME/.homesick/repos/homeshick/homeshick.sh" ]; then
  source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fi
if [ -n "${commands[direnv]}" ]; then
  eval "$(direnv hook zsh)"
fi
if [ -n "${commands[envoy]}" ]; then
  eval "$(envoy -pt ssh-agent)"
fi
if [ -f /usr/share/chruby/chruby.sh ]; then
  source /usr/share/chruby/chruby.sh
  source /usr/share/chruby/auto.sh
elif [ -d "$HOME/.rvm/bin" ]; then
  export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
fi
if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
fi
if [ -d "$HOME/.deer" ]; then
  fpath=($HOME/.deer $fpath)
  autoload -U deer
  zle -N deer
  bindkey '\ek' deer
fi
