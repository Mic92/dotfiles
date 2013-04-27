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
source $HOME/.zprompt
source $HOME/.z/z.sh
source $HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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

ff() { /usr/bin/find . -iname "*$@*" }

function {emacs,ee}merge() {
  if [ $# -ne 2 ]; then
    echo Usage: $0 local base other
    exit 1
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

chpwd() {
  update_terminal_cwd
  ls --color
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

xalias ag='ag --color --ignore \*.min.js'

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

# Editors
[[ -n ${commands[vi]} ]] && alias vi=vim
alias svim='sudo vim'
alias svimdiff='sudo vimdiff'
alias ntree='vim -c NERDTree'
xalias ee="emacs -nw"

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
xhashd vicious=$HOME/.config/awesome/vicious
xhashd dotfiles=$HOME/git/dotfiles
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
alias hping="ping higgsboson.tk"
alias :q=exit

alias webtunnel='ssh higgs-boson-tunnel cat'
alias homesick="$HOME/.homeshick"
# }}}

