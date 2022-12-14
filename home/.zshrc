# early, fast invocation of tmux
# - only if tmux is installed
# - not in linux ttys
# - no nested tmux sessions
if [[ -n ${commands[tmux]} ]] && [[ "$TERM" != "linux" ]] && [[ "$TERM_PROGRAM" != WezTerm ]] && [[ -z "$TMUX" ]] && [[ "$INSIDE_EMACS" != "vterm" ]]; then
  if [[ -n "$SSH_AUTH_SOCK" ]]; then
    tmux set-environment -g SSH_AUTH_SOCK "$SSH_AUTH_SOCK" 2>/dev/null
  fi
  tmux new-session -s "${TTY:t}" -t main || tmux attach-session -t "${TTY:t}"
fi
if [[ -f ~/.nix-profile/etc/profile.d/hm-session-vars.sh ]]; then
  source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
fi

if [[ -e /etc/profile.d/nix.sh ]]; then
  # shellcheck disable=SC1091
  . /etc/profile.d/nix.sh
fi
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
  # shellcheck disable=SC1091
  . ~/.nix-profile/etc/profile.d/nix.sh
fi
if [[ -d ~/git/nixpkgs ]]; then
  export NIX_PATH="nixpkgs=$HOME/git/nixpkgs:$NIX_PATH"
fi
if [[ -d ~/.nix-defexpr/channels ]]; then
  export NIX_PATH="$NIX_PATH:$HOME/.nix-defexpr/channels"
fi
if [[ $OSTYPE == darwin* ]]; then
  export NIX_PATH="$NIX_PATH:darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix"
  if [[ -d ~/git/nix-darwin ]]; then
    export NIX_PATH="$NIX_PATH:darwin=$HOME/git/nix-darwin"
  fi
fi

if [[ -S /nix/var/nix/daemon-socket/socket ]]; then
  export NIX_REMOTE=daemon
fi

function faketty {
  script -qfc "$(printf "%q " "$@")";
}

export NIX_USER_PROFILE_DIR=${NIX_USER_PROFILE_DIR:-/nix/var/nix/profiles/per-user/${USER}}
export NIX_PROFILES=${NIX_PROFILES:-$HOME/.nix-profile}

if [[ -z "$TERMINFO_DIRS" ]] || [[ -d $HOME/.nix-profile/share/terminfo ]]; then
  export TERMINFO_DIRS=$HOME/.nix-profile/share/terminfo
fi

function string_hash() {
  local hashstr=$1
  local hashsize=$2
  local hashval=52

  for i in {1..${#hashstr}}; do
    local thischar=$hashstr[$i]
    hashval=$(( $hashval + $((#thischar)) ))
  done

  # Avoid 0 as that's black
  hashsize=$(( $hashsize - 1 ))
  hashval=$(( $hashval % $hashsize ))
  hashval=$(( $hashval + 1 ))

  echo $hashval
}

if [[ -n $HOST && "$__host__" != "$HOST" ]]; then
  tmux set -g status-bg "colour$(string_hash "$HOST" 255)"
  export __host__=$HOST
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

  words=(${(z)val})
  cmd=${words[1]}

  [[ -n ${commands[$cmd]}  ]] && alias -- "${key}=${val}"
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
upfind() {
  local previous=
  local current=$PWD

  if [[ $# -ne 1 ]];then
    echo "$0 FILE_NAME"
    return 1
  fi

  while [[ -d "$current" && "$current" != "$previous" ]]; do
    local target_path=$current/$1
    if [[ -f "$target_path" ]]; then
      echo "$target_path"
      return 0
    else
      previous=$current
      current=$current:h
    fi
  done
  return 1
}
clone(){
  [[ $# -eq 0 ]] && echo "clone <GIT_CLONE_URL>" && return 1

  cd "$(mktemp -d)" || return 1
  git clone --depth=1 "$1"
}

rust-doc(){
  xdg-open "$(nix-build '<nixpkgs>' -A rustc.doc --no-out-link)/share/doc/rust/html/index.html"
}

wttr() {
  local request="wttr.in/${1-muc}"
  [ "$COLUMNS" -lt 125 ] && request+='?n'
  curl -H "Accept-Language: ${LANG%_*}" --compressed "$request"
}
kpaste() {
  arg=cat
  if [[ $# -ne 0 ]]; then
    arg+=("${@}")
  elif [[ -t 0 ]] && [[ -o interactive ]]; then
    arg=(wl-paste)
  fi
  "${arg[@]}" | curl -sS http://p.r --data-binary @- | \
    sed '$ {p;s|http://p.r|https://p.krebsco.de|}'
}

hm(){
  nix run "$HOME/.homesick/repos/dotfiles#hm" -- "$@"
}
nix-index-update() {
  filename="index-x86_64-$(uname | tr A-Z a-z)"
  mkdir -p ~/.cache/nix-index
  pushd ~/.cache/nix-index
  # -N will only download a new version if there is an update.
  wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename
  ln -f $filename files
  popd
}

## Options
setopt auto_name_dirs
setopt transient_rprompt
setopt pushd_ignore_dups
setopt no_beep
setopt auto_cd
setopt correct
setopt multios
setopt cdablevarS
setopt extended_glob
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
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
# open current line in editor
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

## Completion
autoload colors; colors;
autoload -zU compinit
fignore=(.DS_Store $fignore)
[[ -d ~/.zsh-completions/src ]] && fpath+=(~/.zsh-completions/src)
[[ -d ~/.nix-profile/share/zsh/site-functions ]] && fpath+=(~/.nix-profile/share/zsh/site-functions)
[[ -d /run/current-system/sw/share/zsh/site-functions/ ]] && fpath+=(/run/current-system/sw/share/zsh/site-functions/)

# only update zsh completion once a day
if [[ -n ${ZDOTDIR:-${HOME}}/$ZSH_COMPDUMP(#qN.mh+24) ]]; then
  compinit -d $ZSH_COMPDUMP
else
  compinit -C
fi
zmodload -i zsh/complist
setopt complete_in_word
unsetopt always_to_end
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
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:processes' command "ps -u $(whoami) -o pid,user,comm -w -w"

## Prompt
PURE_GIT_UNTRACKED_DIRTY=0 PURE_GIT_PULL=0

PURE_PROMPT_SYMBOL="%F{blue}╰─ %(?.%F{green}.%F{red})%%%f"
source $HOME/.zsh-pure/async.zsh
source $HOME/.zsh-pure/pure.zsh
zstyle :prompt:pure:path color yellow
zstyle :prompt:pure:git:branch color yellow
zstyle :prompt:pure:user color cyan
zstyle :prompt:pure:host color yellow
zstyle :prompt:pure:git:branch:cached color red
# non-zero exit code in right prompt
RPS1='%(?.%F{magenta}.%F{red}(%?%) %F{magenta})'

## Aliases
# Basic commands
alias zcat='zcat -f'
alias dd='dd status=progress'
if [[ -n ${commands[rg]} ]]; then
  rg() {
    command rg --sort path --pretty --smart-case --fixed-strings "$@" | less -R
  }
  ag() {
    echo "use rg instead"
    sleep 1
    rg "$@"
  }
elif [[ -n ${commands[ag]} ]]; then
  alias ag='ag --color --smart-case --literal --pager=less'
fi
if [[ -n ${commands[zoxide]} ]]; then
  eval "$(zoxide init zsh)"
else
  alias z="builtin cd"
fi
alias pgrep='pgrep -a'

# System tools
xalias top='htop'
xalias lg='lazygit'
xalias dig='q'
alias free='free -m'
alias fuser="fuser -v"
if [[ -n ${commands[dust]} ]]; then
  du() {
    args=()
    for i in "$@"; do
      case "$i" in
      -h|-s|-sh) continue;;
      esac
      args+=("$i")
    done
    dust "${args[@]}"
  }
else
  alias du='du -hc'
fi
alias df='df -hT'
# File management
if [[ -n ${commands[lsd]} ]]; then
  if [ -n "${commands[vivid]}" ]; then
    export LS_COLORS="$(vivid generate dracula)"
  fi
  alias ls="lsd --classify --date=relative"
elif [[ $OSTYPE == freebsd* ]] ||  [[ $OSTYPE == darwin* ]]; then
  alias ls='ls -G'
else
  alias ls='ls --color=auto --classify --human-readable'
fi
alias sl=ls
alias tempdir='cd $(TMPDIR=/tmp mktemp -d);'
alias rm='rm -rv'
function cp() {
  if [[ "$#" -ne 1 ]] || [[ ! -f "$1" ]]; then
    command cp --reflink=auto -arv "$@"
    return
  fi
  newfilename="$1"
  vared newfilename
  command cp --reflink=auto -arv -- "$1" "$newfilename"
}
alias ln="nocorrect ln"
function mv() {
  if [[ "$#" -ne 1 ]] || [[ ! -e "$1" ]]; then
    command mv -v "$@"
    return
  fi

  newfilename="$1"
  vared newfilename
  command mv -v -- "$1" "$newfilename"
}

alias mkdir='nocorrect mkdir -p'
ip() {
    if [[ $# -eq 0 ]]; then
        command ip -c -br a
    else
        command ip -c "$@"
    fi
}
xalias objdump='objdump -M intel'
alias wget='noglob wget'
alias curl='noglob curl --compressed --proto-default https'
alias nix='noglob nix'
alias nom='noglob nom'
alias nixos-remote='noglob nixos-remote'
alias nixos-rebuild='noglob nixos-rebuild'
if [[ -n ${commands[hub]} ]]; then
  alias git='noglob hub'
else
  alias git='noglob git'
fi
alias rake='noglob rake'
# Root
# fallback if sudo is not yet installed
alias su='su - '
xalias ctl='sudo systemctl'
alias gdb='gdb --quiet --args'
alias readelf='readelf -W'
# Editors
[[ -n ${commands[vi]} ]] && alias vi=vim
xalias vim="nvim"
xalias xclip="xclip -selection clipboard"
xalias cloc=scc

if [[ -n ${commands[nix]} ]]; then
  n() {
    NIX_RUN_ARGS="$@${NIX_RUN_ARGS+ }${NIX_RUN_ARGS}" nix shell "$@" -f '<nixpkgs>' -c zsh
  }
  nbuild() {
    nix build --no-link "$@"
    nix path-info "$@"
  }
fi

nix-call-package() {
    if [ $# != 1 ]; then
        echo "USAGE: $0" >&2
        return 1
    fi
    nix-build -E "with import <nixpkgs> {}; pkgs.callPackage $1 {}"
}

nix-pkg-path() {
    if [ $# != 1 ]; then
        echo "USAGE: $0" >&2
        return 1
    fi
    nix-shell -p "$1" --run 'echo $buildInputs'
}

nix-unpack() {
    if [ $# != 1 ]; then
        echo "USAGE: $0" >&2
        return 1
    fi
    pkg=$1
    nix-shell \
      -E "with import <nixpkgs> {}; mkShell { buildInputs = [ (srcOnly pkgs.\"$pkg\") ]; }" \
      --run "cp -r \$buildInputs $pkg; chmod -R +w $pkg"
}

killp() {
  local pid=$(ps -ef | sed 1d | eval "fzf ${FZF_DEFAULT_OPTS} -m --header='[kill:process]'" | awk '{print $2}')
  if [[ "$pid" != "" ]]; then
    echo $pid | xargs sudo kill -${1:-9}
    killp
  fi
}

# Dir Hashes
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
# generic aliases
# diff format like git
xalias diff='diff -Naur --strip-trailing-cr'
[ -z "${commands[ping6]}" ] && alias ping6="ping -6"
alias :q=exit
alias grep="grep --binary-files=without-match --directories=skip --color=auto"
alias R="R --quiet"
alias strace="strace -yy"

if [ -n "${commands[bat]}" ]; then
  cat() {
    if [[ -t 1 ]] && [[ -o interactive ]]; then
        if [[ -n "$WAYLAND_DISPLAY" ]]; then
            wl-copy < "$1" 2>/dev/null &
        elif [[ -n "$DISPLAY" ]]; then
            xclip -selection clipboard < "$1" 2>/dev/null &
        fi
        bat "$@"
    else
        command cat "$@"
    fi
  }
fi

## PROFILE
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $HOME/.cargo/bin
    $HOME/go/bin
    $HOME/.go/bin
    $HOME/.gem/ruby/*.*.*/bin(NOn[1])
    # python
    $HOME/.local/bin/
    $path
)
# get rid of duplicate
typeset -U path
# remove non-existing entries from path
path=($^path(N))
export PATH
cdpath=( ~/git )
# Prefered programs
export BROWSER=firefox
export TERMINAL=wezterm
export PICTUREVIEW=eog
if [[ -n ${commands[emacseditor]} ]] && [[ -n $XDG_RUNTIME_DIR ]]; then
  export EDITOR=emacseditor
  alias vim=emacseditor
else
  export EDITOR=vim
fi

if [[ -n ${command[nvim]} ]]; then
    export ALTERNATE_EDITOR=nvim
elif [[ -n ${command[vim]} ]]; then
    export ALTERNATE_EDITOR=vim
fi

export VISUAL=$EDITOR
export PAGER=less
export ACK_PAGER=$PAGER
export READNULLCMD=$PAGER
export pacman_program=pacman-color
# X11, Sound, Graphic
export XDG_CACHE_HOME=~/.cache
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.data
export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"
export XDG_MUSIC_DIR="$HOME/Music"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_PUBLICSHARE_DIR="$HOME/Public"
export XDG_TEMPLATES_DIR="$HOME/.Templates"
export XDG_VIDEOS_DIR="$HOME/Videos"
export ERRFILE=~/.xsession-errors
# Antialising
export _JAVA_OPTIONS="$_JAVA_OPTIONS -Dawt.useSystemAAFontSettings=lcd -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
# Enable Pipewire for SDL
export SDL_AUDIODRIVER=pipewire
export ALSOFT_DRIVERS=pipewire
# fix broken xdg-open
export GDMSESSION=1 GNOME_DESKTOP_SESSION_ID=1
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
if [[ $OSTYPE == darwin* ]] || [[ $OSTYPE == freebsd* ]]; then
  export LC_ALL=en_US.UTF-8
else
  export LC_ALL=en_DK.UTF-8
fi
export PERL_CPANM_OPT="--local-lib=~/.perl5"
export PERL5LIB=~/.perl5/lib/perl5

# downgrade terminfo to tmux if we are inside
if [[ -n "$TMUX" ]] && [[ -n "${commands[tput]}" ]]; then
    if TERM=tmux-256color tput longname >/dev/null 2>&1 ; then
        export TERM=tmux-256color
    else
        export TERM=screen-256color
    fi
fi

export GOPATH="$HOME/go"
[[ ! -d "$GOPATH" ]] && mkdir -p "$GOPATH/src" 2>/dev/null

if [[ -S "/run/user/${UID}/ssh-agent" ]]; then
  export SSH_AUTH_SOCK="/run/user/${UID}/ssh-agent"
fi

unlock_root(){
  pw=$(rbw get 'zfs encryption')
  ssh root@eve.i -p 2222 "zpool import -a; echo "${pw}" | zfs load-key -a; echo "${pw}" | zfs load-key -a; killall zfs"
}
# Autoinstall Bundle
bundle() {
  if [[ -z "${commands[bundle]}" ]] && [[ -n "${commands[gem]}" ]]; then
   gem install --user-install bundler
  fi
  command bundle "$@"
}
fd() {
  if [[ -n "${commands[fd]}" ]]; then
    command fd "$@"
  else
    command find . -iname "*${*}*" 2>/dev/null
  fi
}
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
say() {
  _say() { curl -sSG http://tts.r/api/tts --data-urlencode text@- | mpv --no-resume-playback -; }
  if [[ "$#" -eq 0 ]]; then
    _say
  else
    echo "$@" | _say
  fi
}
own() {
  if [[ -n "${commands[sudo]}" ]]; then
    sudo chown -R "$USER:$(id -gn)" "$@"
  else
    chown -R "$USER:$(id -gn)" "$@"
  fi
}
# usage
# vil file:20 -> opens file on line 20
vil() {
  setopt shwordsplit
  IFS=':' ARGS=($@)
  unsetopt shwordsplit
  vim +${ARGS[2]} ${ARGS[1]}
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
sieve-edit() {
    local passwordfd
    exec {passwordfd} < <(rbw get Eve)
    nix shell -f '<nixpkgs>' sieve-connect -c sieve-connect --passwordfd $passwordfd -s imap.thalheim.io -u joerg@higgsboson.tk --remotesieve Filter --edit
    exec {passwordfd}>&-
}
# Autossh - try to connect every 0.5 secs (modulo timeouts)
sssh(){ while true; do command ssh -q "$@"; [ $? -ne 0 ] && break || sleep 0.5; done; }
dumbssh(){ TERM=screen-256color ssh "$@"; }
moshlogin(){
  if ssh-add -L | grep -q "no identities"; then
    ssh-add ~/.ssh/id_{rsa,ecdsa,ed25519}
  fi
  ssh -v eve killall mosh-server
  mosh -A eve.mosh
}
# List directory after changing directory
chpwd() { ls; }

# OSC-133
precmd() {
  print -Pn "\e]133;A\e\\"
}

function osc7 {
    local LC_ALL=C uri input
    export LC_ALL

    setopt localoptions extendedglob
    input=( ${(s::)PWD} )
    uri=${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%${(l:2::0:)$(([##16]#match))}}
    print -n "\e]7;file://${HOSTNAME}${uri}\e\\"
}
add-zsh-hook -Uz chpwd osc7

mkcd() { mkdir -p "$1" && cd "$1"; }
# make cd accept files
cd() {
  if [[ "$1" == "--" ]]; then
    shift
  fi
  local to="${1:-$HOME}"
  if [[ -f "$to" ]]; then
    to="$(dirname $to)"
  fi

  # fallback to zoxide if builtin cd fails
  if ! builtin cd "$to" && [[ -n ${commands[zoxide]} ]]; then
    __zoxide_z "$to"
  fi
}
pwd() {
    if [[ -t 1 ]] && [[ -o interactive ]]; then
        if [[ -n "$WAYLAND_DISPLAY" ]]; then
            echo $PWD | wl-copy
        elif [[ -n "$DISPLAY" ]]; then
            echo $PWD | xclip -selection clipboard
        fi
    fi
    builtin pwd
}
urlencode() { python3 -c "import sys, urllib.parse as parse; print(parse.quote(sys.argv[1]))" $1; }
urldecode() { python3 -c "import sys, urllib.parse as parse; print(parse.unquote(sys.argv[1]))" $1; }
cheat() { command cheat -c "$@" | less; }
ninja(){
  local build_path="$(dirname "$(upfind "build.ninja")")"
  command ninja -C "${build_path:-.}" "$@"
}
make(){
  local build_path="$(dirname "$(upfind "Makefile")")"
  command make -C "${build_path:-.}" "$@"
}
cargo(){
  local build_path="$(dirname "$(upfind "Cargo.toml")")"
  (
    builtin cd "${build_path:-.}" >/dev/null || true
    command cargo "$@"
  )
}
real-which(){
  readlink -f "$(command which "$@")"
}
copypath() {
  p=$(realpath "${1:-.}")
  if [[ -n "$WAYLAND_DISPLAY" ]]; then
    echo "$p" | wl-copy 2>/dev/null
  elif [[ -n "$DISPLAY" ]]; then
    echo "$p" | xclip -selection clipboard 2>/dev/null
  fi
  echo "$p"
}

untilport(){
  if [[ $# -lt 2 ]]; then
    echo "$0: host port"
    return 1
  fi
  until nc -z "$@"; do sleep 1; done
}

nixify() {
  if [ ! -e ./.envrc ]; then
    echo "use nix" > .envrc
    direnv allow
  fi
  if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
    cat > default.nix <<'EOF'
with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    bashInteractive
  ];
}
EOF
    ${EDITOR:-vim} default.nix
  fi
}

flakify() {
  if [ ! -e flake.nix ]; then
    nix flake new -t github:hercules-ci/flake-parts .
  elif [ ! -e .envrc ]; then
    echo "use flake" > .envrc
    direnv allow
  fi
  ${EDITOR:-vim} flake.nix
}

open() {
  if [[ -n "${commands[xdg-open]}" ]]; then
    xdg-open "$@"
  elif [[ -n "${commands[kde-open5]}" ]]; then
    kde-open5 "$@"
  elif [[ -n "${commands[gnome-open]}" ]]; then
    gnome-open "$@"
  else
    echo "no suitable command found" >&2
    return 1
  fi
}
fixssh() {
  for key in SSH_AUTH_SOCK SSH_CONNECTION SSH_CLIENT; do
    if (tmux show-environment | grep "^${key}" > /dev/null); then
      value=$(tmux show-environment | grep "^${key}" | sed -e "s/^[A-Z_]*=//")
      export ${key}="${value}"
    fi
  done
}

tmux-upterm() {
  upterm host --server ssh://upterm.thalheim.io:2323 \
    --force-command 'tmux attach -t pair-programming' \
    -- bash -c "read -p 'Press enter to continue ' && tmux new -t pair-programming"
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
if [[ -f "$HOME/.zshrc.$HOST" ]]; then
  source "$HOME/.zshrc.$HOST"
fi

## Plugins
if [[ -f ~/.zsh-history-substring-search/zsh-history-substring-search.zsh ]]; then
  source ~/.zsh-history-substring-search/zsh-history-substring-search.zsh
  # bind P and N for EMACS mode
  bindkey -M emacs '^P' history-substring-search-up
  bindkey -M emacs '^N' history-substring-search-down
fi
if [[ -f ~/.zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
  source ~/.zsh-autosuggestions/zsh-autosuggestions.zsh
  export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE=fg=60
fi
if [[ -f ~/.homesick/repos/homeshick/homeshick.sh ]]; then
  source ~/.homesick/repos/homeshick/homeshick.sh
fi
if [[ -f ~/.zsh-autopair/autopair.zsh ]]; then
  source ~/.zsh-autopair/autopair.zsh
fi
source ~/.zsh-termsupport

if [ -n "${commands[r2]}" ]; then
  r2() {
    if [[ "$#" -eq 0 ]]; then
      command r2 -
    else
      command r2 "$@"
    fi
  }
fi

if [ -n "$WAYLAND_DISPLAY" ]; then
  alias chromium="chromium --enable-features=UseOzonePlatform --ozone-platform=wayland"
fi
if [ -n "${commands[direnv]}" ]; then
  eval "$(direnv hook zsh)"
fi
if [[ $commands[kubectl] ]]; then
   alias k=kubectl
   source <(kubectl completion zsh)
fi
alias tf=terraform

if [[ -n "${commands[fzf-share]}" ]]; then
  FZF_CTRL_R_OPTS=--reverse
  source "$(fzf-share)/key-bindings.zsh"
fi
if [[ -f ~/.fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ]]; then
  source ~/.fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fi


# prevent broken terminals
ttyctl -f
