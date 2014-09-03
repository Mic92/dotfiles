# early, fast invocation of tmux
# - only if tmux is installed
# - not in linux ttys
# - no nested tmux sessions
if [[ -n ${commands[tmux]} && "$TERM" != "linux" && -z "$TMUX" ]]; then
  tmux attach-session || tmux
  [[ $? = "0" ]] && exit
fi

source $HOME/.zshuery/zshuery.sh
load_defaults
load_aliases
load_completion $HOME/.zsh-completion

bindkey -e
source "$HOME/.zprompt"
source "$HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$HOME/.zsh-history-substring-search/zsh-history-substring-search.zsh"
source "$HOME/.zsh-autosuggestions/autosuggestions.zsh"

# {{{ Functions
shorturl() {
  if [[ $# -ne 1 && $# -ne 2 ]]; then
    echo "Usage: $0 URL [TITLE]"
    return 1
  fi

  if [ -z $YOURLS_TOKEN ]; then
    echo "Please set the YOURLS_TOKEN environment variable"
    return 1
  fi

  local short_url
  short_url=$(curl --silent --show-error -X POST \
    --data-urlencode "signature=$YOURLS_TOKEN" \
    --data-urlencode "action=shorturl" \
    --data-urlencode "url=$1" \
    --data-urlencode "keyword=$2" \
    --data-urlencode "format=simple" \
    http://higgs.tk/yourls-api.php)
  echo $short_url | xclip
  echo $short_url
}

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

browse () {
  if [[ $BROWSER == opera ]]; then
    opera -remote "openURL(file://`pwd`/$1,new-page)"
  else
    $BROWSER file://"`pwd`/$1"
  fi
}

# }}}

# {{{ Helpers
# {{{ xalias - only supposed to be used with simple aliases.
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
# }}}

# {{{ xhashd - check for directory, then create hash -d
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
# }}}

# check_com - check if a command exists {{{
# eg: check_com "vim -p"
function check_com() {
    #setopt localoptions xtrace
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
# }}}
# }}}

# {{{ Aliase
# Basic commands
alias zcat='zcat -f'
# - same as tail but using inotify for better perfomance
# - linux only
xalias tail='inotail'
alias less='less -isRM'

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
alias ls='ls --color=auto -F -h'
#alias sl=ls
alias la='ls -lA'
alias laa='ls -la'
alias ll='ls -l'
alias tempdir='cd `mktemp -d`'

alias rm='rm -rv'
alias cp='nocorrect cp -rpv'
alias cpv="rsync -pogr --progress"
alias ln="nocorrect ln"
alias mv='nocorrect mv -v'
alias mkdir='nocorrect mkdir -p'
xalias locate='locate --existing --follow --basename --ignore-case'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

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

ulimit -S -c 0 # disable core dumps
stty -ctlecho # turn off control character echoing

if [[ $TERM = linux ]]; then
  setterm -regtabs 2 # set tab width of 4 (only works on TTY)
fi

# Editors
[[ -n ${commands[vi]} ]] && alias vi=vim
alias svim='sudo vim'
alias svimdiff='sudo vimdiff'
alias ntree='vim -c NERDTree'
xalias ee="emacs -nw"

xalias mplayer="mpv"

# reboot/halt/suspend
alias shutdown='! shutdown'
alias reboot='! reboot'
xalias shutdown="systemctl poweroff"
xalias reboot="systemctl reboot"
xalias suspend="systemctl suspend"
xalias hibernate="systemctl hibernate"
xalias hybrid-sleep="systemctl hybrid-sleep"

# Package management
if [[ -f /etc/debian_version ]] ; then
  alias apt-get='sudo apt-get'
  alias apt-secure='sudo apt-secure'
  alias apt-file='sudo apt-file'
  alias aptitude='sudo aptitude'
  alias dpkg='sudo dpkg'
  alias update='apt-get update && apt-get upgrade'
  alias dist-upgrade='apt-get dist-upgrade'
  alias install='apt-get install'
  alias remove='apt-get remove'
  alias purge='apt-get purge'
  alias clean='apt-get clean && apt-get autoclean'

  alias search='apt-cache search'
  alias show='apt-cache show'
  alias depends='apt-cache depends'

  alias ifup='sudo ifup'
  alias ifdown='sudo ifdown'
  alias pon='sudo pon'
  alias poff='sudo poff'
  alias plog='sudo plog'
elif [[ -f /etc/arch-release ]] ; then
  # if colored version is avaible
  xalias pacman='pacman-color'
  xalias y=yaourt
  # run as root
  alias pacman='sudo pacman'
  alias pacman-optimize='sudo pacman-optimize'
  alias abs='sudo abs'
  # pacman-contrib stuff
  xalias pacdiff='sudo pacdiff -l'
  xalias pacscripts='sudo pacscripts'
  xalias pactree='pactree -c'
  xhashd yaourtbuild=/var/abs/local/yaourtbuild
  xhashd clydebuild=/var/abs/local/clydebuild
elif [[ -f /etc/gentoo-release ]] ; then
  alias emerge='sudo emerge'
  xalias eix-sync='sudo eix-sync -C --quiet'
fi

# Set up auto extension stuff
alias -s exe=wine

# Dir Hashes
xhashd awesome=$HOME/.config/awesome/
xhashd xmonad=$HOME/.xmonad
xhashd vicious=$HOME/.config/awesome/vicious
xhashd dotfiles=$HOME/git/dotfiles
xhashd git=$HOME/git
xhashd doc=/usr/share/doc
xhashd abs=/var/abs
xhashd web=/var/www
xhashd web=/srv/http
xhashd log=/var/log
xhashd zdir=${${${(M)fpath:#*/zsh/${ZSH_VERSION}/*}[1]}%${ZSH_VERSION}*}${ZSH_VERSION}

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
alias aliases='less ~/.zaliases'
# a fresh zsh, please.
alias fzsh='PS1="zsh%# " zsh -f'
# generic aliases
xalias rot13='tr A-Za-z N-ZA-Mn-za-m'
# diff format like git
xalias diff='diff -Naur --strip-trailing-cr'
alias gping="ping google.com"
alias gping6="ping6 google.com"
alias ghost="host -v google.com 8.8.8.8"
alias gcurl="curl -v google.com"
alias :q=exit
alias todotxt="vim ~/Dropbox/todo/todo.txt"

alias pacunlock="sudo rm /var/lib/pacman/db.lck"

[ -n ${commands[pacman]} ] && function owns() { /usr/bin/pacman -Qo $(which $1)}

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

function print_result() {
  subsystem=$1
  result=$2
  bold=`tput bold`
  normal=`tput sgr0`
  if [[ $result -eq "0" ]]; then
    echo "$bold$subsystem:$normal \e[0;32m✓\e[0m"
  else
    echo "$bold$subsystem:$normal \e[0;31m✗\e[0m"
  fi
}

function netcheck() {
  echo "Test Ping (IPv4)"
  ping -c 3 8.8.8.8
  ipv4=$?
  echo "Test DNS (Google DNS)"
  host github.com 8.8.8.8
  dns_google=$?
  echo "Test DNS (Higgsboson.tk DNS)"
  host github.com 82.196.5.246
  dns_higgsboson=$?
  echo "Test HTTP (IPv4)"
  curl -v google.com >/dev/null
  http=$?
  echo "Test Ping (IPv6)"
  ping6 -c 3 ipv6.google.com
  ipv6=$?

  echo ">>>>Result<<<<"
  print_result "ipv4" $ipv4
  print_result "ipv6" $ipv6
  print_result "dns (google)" $dns_google
  print_result "dns (higgsboson.tk)" $dns_higgsboson
  print_result "http" $http
}

alias webtunnel='ssh higgs-boson-tunnel cat'

if [ -e $HOME/.homesick/repos/homeshick/homeshick.sh ]; then
  source $HOME/.homesick/repos/homeshick/homeshick.sh
fi

if [[ -n ${commands[envoy]} ]] && (( UID != 0)); then
  envoy
  source <(envoy --agent=gpg-agent --print)
  source <(envoy --agent=ssh-agent --print)
fi

# zprofile not sourced
[ -z $EDITOR ] && [ -f $HOME/.zprofile ] && source $HOME/.zprofile
[ -e $HOME/.zshrc.$HOST ] && source $HOME/.zshrc.$HOST

[ -x /usr/bin/direnv ] && eval "$(direnv hook zsh)"

# Enable autosuggestions automatically
zle-line-init() {
  zle autosuggest-start
}
zle -N zle-line-init

export AUTOSUGGESTION_HIGHLIGHT_COLOR='fg=10'

# persisting the dirstack
DIRSTACKSIZE=${DIRSTACKSIZE:-20}
dirstack_file=${dirstack_file:-${HOME}/.zdirs}
if [[ -f ${dirstack_file} ]] && [[ ${#dirstack[*]} -eq 0 ]] ; then
  dirstack=( ${(f)"$(< $dirstack_file)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi

chpwd() {
  if (( $DIRSTACKSIZE <= 0 )) || [[ -z $dirstack_file ]]; then return; fi
  local -ax my_stack
  my_stack=( ${PWD} ${dirstack} )
  builtin print -l ${(u)my_stack} >! ${dirstack_file}

  update_terminal_cwd
  ls --color
}

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
