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

if [ -f ~/.nix-profile/zsh/ghostty-integration ]; then
  # shellcheck disable=SC1091
  . ~/.nix-profile/zsh/ghostty-integration
fi

if [[ $OSTYPE == darwin* ]]; then
  export NIX_PATH="$NIX_PATH:darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix"
fi

if [[ -S /nix/var/nix/daemon-socket/socket ]]; then
  export NIX_REMOTE=daemon
fi

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
  local request="wttr.in/${1-Neufahrn}"
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
# merge after CI is green with mergify
merge-after-ci() {
  echo "use merge-when-green instead" 2>&1
  return 1
}
passgen() {
  local pass
  pass=$(nix run nixpkgs#xkcdpass -- -d '-' -n 3 -C capitalize "$@")
  echo "${pass}$((RANDOM % 10))"
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
bindkey "^[[3~" delete-char # bind delete key

## Completion
autoload colors; colors;
autoload -Uz compinit
compinit -C

if [[ -n ${commands[fzf]} ]]; then
  source ~/.zsh-fzf-tab/fzf-tab.zsh
  if [ -n $TMUX ]; then
    zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
  fi
  # disable sort when completing `git checkout`
  zstyle ':completion:*:git-checkout:*' sort false
  # set descriptions format to enable group support
  # NOTE: don't use escape sequences here, fzf-tab will ignore them
  zstyle ':completion:*:descriptions' format '[%d]'
  # set list-colors to enable filename colorizing
  zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
  # force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
  zstyle ':completion:*' menu no
  # preview directory's content with lsd when completing cd
  if [[ -n ${commands[lsd]} ]]; then
    zstyle ':fzf-tab:complete:cd:*' fzf-preview 'lsd -1 --color=always $realpath'
  fi
  # switch group using `<` and `>`
  zstyle ':fzf-tab:*' switch-group '<' '>'
fi

fignore=(.DS_Store $fignore)
[[ -d ~/.zsh-completions/src ]] && fpath+=(~/.zsh-completions/src)
[[ -d ~/.nix-profile/share/zsh/site-functions ]] && fpath+=(~/.nix-profile/share/zsh/site-functions)
[[ -d /run/current-system/sw/share/zsh/site-functions/ ]] && fpath+=(/run/current-system/sw/share/zsh/site-functions/)

setopt complete_in_word
unsetopt always_to_end

## Prompt
PURE_GIT_UNTRACKED_DIRTY=0 PURE_GIT_PULL=0
#
PURE_PROMPT_SYMBOL="%(?.%F{green}.%F{red})%%%f"
fpath+=($HOME/.zsh-pure)
zstyle :prompt:pure:path color yellow
zstyle :prompt:pure:git:branch color yellow
zstyle :prompt:pure:user color cyan
zstyle :prompt:pure:host color yellow
zstyle :prompt:pure:git:branch:cached color red
## non-zero exit code in right prompt
RPS1='%(?.%F{magenta}.%F{red}(%?%) %F{magenta})'
autoload -U promptinit; promptinit
prompt pure

# Load direnv-instant integration for non-blocking prompt
if [ -n "${commands[direnv-instant]}" ] && [ -n "${commands[direnv-instant]}" ]; then
  eval "$(direnv-instant hook zsh)"
fi


## Aliases
# Basic commands
alias zcat='zcat -f'
alias dd='dd status=progress'


jt() {
  local tmpdir
  tmpdir=$(mktemp -d)
  trap 'rm -rf "$tmpdir"' EXIT
  lazyworktree -output-selection "$tmpdir/out" "$@"
  dir=$(<"$tmpdir/out")
  echo "$dir"
  if [[ -d  "$dir" ]]; then
    cd "$dir"
    if [ -f .envrc ]; then
      direnv allow
    fi
  fi
  unset dir
}

rg() {
  local pager=($PAGER)

  if [[ -n ${commands[delta]} ]]; then
    pager=(delta --hyperlinks)
  fi

  if [[ -n ${commands[rg]} ]]; then
    (
      command rg --sort path --smart-case --fixed-strings --json -C 2 "$@";
      if [[ -t 0 ]]; then
        command rg --files | command rg --no-line-number --json -C 2 "$@"
      fi) | $pager
  else
    grep -r -C 2 "$@"
  fi
}
if [[ -n ${commands[zoxide]} ]]; then
  eval "$(zoxide init zsh)"
fi
if [[ -n ${commands[nom-build]} ]]; then
  alias nix-build=nom-build
  alias nix=nom
fi
alias pgrep='pgrep -a'

# System tools
xalias top='htop'
xalias lg='lazygit'
xalias dig='q'
alias free='free -g'
alias fuser="fuser -v"
if [[ -n ${commands[procs]} ]]; then
  alias ps='procs --theme light'
else
  alias ps='ps auxf'
fi
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
if [[ -n ${commands[eza]} ]]; then
  alias ls="eza --icons --smart-group --time-style relative"
  alias tree="eza --tree --icons"
elif [[ $OSTYPE == freebsd* ]] ||  [[ $OSTYPE == darwin* ]]; then
  alias ls='ls -G'
else
  alias ls='ls --color=auto --classify --human-readable'
fi
alias sl=ls
function tempdir() {
  local random_adjective=$(shuf -n 1 $HOME/.zsh/random-adjective.txt)
  local random_name=$(shuf -n 1 $HOME/.zsh/random-name.txt)

  cd "$(mktemp -d "/tmp/$random_adjective-$random_name-XXXXXX")"
}
alias rm='rm -rv'
function cp() {
  if [[ "$#" -ne 1 ]] || [[ ! -f "$1" ]]; then
    if [[ -n ${commands[xcp]} ]]; then
      # Check if -r or --recursive is already in the arguments
      local has_recursive=0
      for arg in "$@"; do
        if [[ "$arg" == "-r" ]] || [[ "$arg" == "--recursive" ]] || [[ "$arg" =~ ^-[^-]*r ]]; then
          has_recursive=1
          break
        fi
      done
      if [[ $has_recursive -eq 0 ]]; then
        set -- -r "$@"
      fi
      command xcp "$@"
    else
      command cp --reflink=auto -arv "$@"
    fi
    return
  fi
  newfilename="$1"
  vared newfilename
  if [[ -n ${commands[xcp]} ]]; then
    command xcp -r "$1" "$newfilename"
  else
    command cp --reflink=auto -arv -- "$1" "$newfilename"
  fi
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
    colour=-c
    brief=-br
    if [[ $OSTYPE == darwin* ]]; then
      # iproute2mac
      colour=
      brief=
    fi
    if [[ $# -eq 0 ]]; then
        command ip $colour $brief a
    else
        command ip $colour "$@"
    fi
}
xalias objdump='objdump -M intel'
alias curl='noglob curl --compressed --proto-default https'
alias nix='noglob nix'
alias nom='noglob nom'
nixos-rebuild() {
  # do not use sudo if --target-host is passed
  if [[ " $* " == *" --target-host "* ]]; then
    noglob command nixos-rebuild "$@"
    return
  fi
  noglob sudo nixos-rebuild
}
alias wget='noglob wget --continue --show-progress --progress=bar:force:noscroll'
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
xalias xclip="xclip -selection clipboard"
xalias cloc=scc

if [[ -n ${commands[sgpt]} ]]; then
  sgpt() {
    export OPENAI_API_KEY=$(rbw get openai-api-key)
    command sgpt "$@"
  }
fi

if [[ -n ${commands[claude]} ]]; then
  alias sonnet='claude --model sonnet'
  alias opus='claude --model opus'
  alias haiku='claude --model haiku'
fi

nix-call-package() {
    if [ $# -lt 1 ]; then
        echo "USAGE: $0" >&2
        return 1
    fi
    file=$1
    shift
    nix-build -E "with import <nixpkgs> {}; pkgs.callPackage $file {}" "$@"
}
nixos-build() {
    if [ $# -lt 1 ]; then
        if [ $OSTYPE = linux* ]; then
          name=$(</proc/sys/kernel/hostname)
        else
          echo "USAGE: $0 name" >&2
          return 1
        fi
    else
        name=$1
        shift
    fi

    command nixos-rebuild build --flake ".#$name" "$@"
}
nixos-eval() {
    if [ $# -lt 1 ]; then
        if [ $OSTYPE = linux* ]; then
          name=$(</proc/sys/kernel/hostname)
        else
          echo "USAGE: $0 name" >&2
          return 1
        fi
    else
        name=$1
        shift
    fi

    command nixos-rebuild dry-build --flake ".#$name" "$@"
}

darwin-build() {
    if [ $# -lt 1 ]; then
      echo "USAGE: $0 name" >&2
    fi
    name=$1
    shift

    command nix build ".#darwinConfigurations.$name.config.system.build.toplevel" "$@"
}

flake-check() {
    if [ $# -lt 1 ]; then
      nix flake check
    fi
    for arg in "$@"; do
      nix build ".#checks.$(nix eval --impure --raw --expr builtins.currentSystem).$1"
    done
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
      -E "with import <nixpkgs> {}; mkShell { buildInputs = [ (srcOnly pkgs.$pkg) ]; }" \
      --run "cp -r \$buildInputs $pkg; chmod -R +w $pkg"
}

killp() {
  local pid=$(procs --color=always "$1" | fzf --ansi -m --header='[kill:process]' --header-lines 2 | awk '{print $1}')
  if [[ "$pid" != "" ]]; then
    echo $pid | xargs sudo kill -${2:-9}
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
alias :q=exit
alias grep="grep --binary-files=without-match --directories=skip --color=auto"
alias R="R --quiet"
alias strace="strace -yy"

if [ -n "${commands[bat]}" ]; then
  cat() {
    if [[ -t 1 ]] && [[ -o interactive ]]; then
        if [[ -n "$WAYLAND_DISPLAY" ]]; then
            wl-copy < "$1" 2>/dev/null &
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
if [ -z "$WAYLAND_DISPLAY" ] || [ -z "$DISPLAY" ]; then
  export BROWSER=echo
elif [ -n "${commands[firefox]}" ]; then
  export BROWSER=firefox
fi
export TERMINAL=ghostty
export PICTUREVIEW=eog

if [[ -n ${commands[nvim]} ]]; then
  export EDITOR=nvim
  alias vim="nvim"
elif [[ -n ${commands[emacseditor]} ]]; then
  export EDITOR=emacseditor
else
  export EDITOR=vim
fi
if [[ -n ${commands[nvim]} ]]; then
  export ALTERNATE_EDITOR=nvim
elif [[ -n ${commands[vim]} ]]; then
  export ALTERNATE_EDITOR=vim
fi

export VISUAL=$EDITOR
if [[ -n ${commands[nvim]} ]]; then
  export MANPAGER="nvim +Man!"
elif [[ -n ${commands[moar]} ]]; then
  export MANPAGER="moar"
  export MOAR='--no-linenumbers --quit-if-one-screen'
  alias less='moar'
elif [[ -n ${commands[less]} ]]; then
  export MANPAGER="less"
fi
if [[ -n ${commands[moar]} ]]; then
  export PAGER="moar"
elif [[ -n ${commands[less]} ]]; then
  export PAGER="less"
fi
export READNULLCMD=$PAGER
# X11, Sound, Graphic
export XDG_DESKTOP_DIR="$HOME/Desktop"
export XDG_CONFIG_HOME="$HOME/.config" # for macos
export XDG_DOCUMENTS_DIR="$HOME/Documents"
export XDG_DOWNLOAD_DIR="$HOME/Downloads"
export XDG_MUSIC_DIR="$HOME/Music"
export XDG_PICTURES_DIR="$HOME/Pictures"
export XDG_PUBLICSHARE_DIR="$HOME/Public"
export XDG_TEMPLATES_DIR="$HOME/.Templates"
export XDG_VIDEOS_DIR="$HOME/Videos"
# Enable Pipewire for SDL
export SDL_AUDIODRIVER=pipewire
export ALSOFT_DRIVERS=pipewire
# less
export LESS=-FXisRM
# Man
export MANWIDTH=80
# If the execution of a command takes longer than
# REPORTTIME (in seconds),  time statistics are printed
export REPORTTIME=4
export LC_ALL=en_US.UTF-8
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
  ssh root@eve.i -p 2222 "echo ${pw} | systemd-tty-ask-password-agent"
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
own() {
  if [[ -n "${commands[sudo]}" ]]; then
    sudo chown -R "$USER:$(id -gn)" "$@"
  else
    chown -R "$USER:$(id -gn)" "$@"
  fi
}
# force output to be on a single line
if [[ $OSTYPE == linux* ]]; then
  ss() {
    # -p requires sudo to see all processes
    if echo "$@" | grep -q "p"; then
      sudo ss "$@" | tee
    else
      command ss "$@" | tee
    fi
  }
fi
sieve-edit() {
  local passwordfd
  password=$(rbw get Eve)
  exec {passwordfd} < <(echo "$password")
  nix run nixpkgs#sieve-connect -- --passwordfd $passwordfd -s imap.thalheim.io -u joerg@thalheim.io --remotesieve Filter --edit
  exec {passwordfd}>&-
}
# Autossh - try to connect every 0.5 secs (modulo timeouts)
sssh(){ while true; do command ssh -q "$@"; [ $? -ne 0 ] && break || sleep 0.5; done; }
# SSH to ephemeral machine without storing host key
ssh-ephermal(){ ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$@"; }

dumbssh(){ TERM=screen-256color ssh "$@"; }
# List directory after changing directory
chpwd() { ls; }

# OSC-133
precmd() {
  print -Pn "\e]133;A\e\\"
}
function osc7-pwd() {
    emulate -L zsh # also sets localoptions for us
    setopt extendedglob
    local LC_ALL=C
    printf '\e]7;file://%s%s\e\' $HOST ${PWD//(#m)([^@-Za-z&-;_~])/%${(l:2::0:)$(([##16]#MATCH))}}
}

function chpwd-osc7-pwd() {
    (( ZSH_SUBSHELL )) || osc7-pwd
}
add-zsh-hook -Uz chpwd chpwd-osc7-pwd

function delta_sidebyside {
  if [[ COLUMNS -ge 140 ]]; then
    export DELTA_FEATURES='side-by-side'
  else
    export DELTA_FEATURES=''
  fi
}
trap delta_sidebyside WINCH


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
reporoot() {
  cd "$(git rev-parse --show-toplevel)"
}
urlencode() { python3 -c "import sys, urllib.parse as parse; print(parse.quote(sys.argv[1]))" $1; }
urldecode() { python3 -c "import sys, urllib.parse as parse; print(parse.unquote(sys.argv[1]))" $1; }
cheat() { command cheat -c "$@" | "$PAGER"; }
ninja(){
  local build_path="$(dirname "$(upfind "build.ninja")")"
  command nice -n19 ninja -C "${build_path:-.}" "$@"
}
if [[ $OSTYPE == linux* ]]; then
  alias cal="cal -m"
fi
make(){
  local build_path="$(dirname "$(upfind "Makefile")")"
  command nice -n19 make -C "${build_path:-.}" -j$(nproc) "$@"
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
  if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
    nix flake new -t github:Mic92/flake-templates#nix-shell .
  elif [ ! -e ./.envrc ]; then
    echo "use nix" > .envrc
  fi
  direnv allow
  ${EDITOR:-vim} default.nix
}
nix-update() {
  if [[ -f $HOME/git/nix-update/flake.nix ]]; then
    nix run $HOME/git/nix-update#nix-update -- "$@"
  else
    nix run nixpkgs#nix-update -- "$@"
  fi
}
nix-fast-build() {
  if [[ -f $HOME/git/nix-fast-build/flake.nix ]]; then
    nix run $HOME/git/nix-fast-build#nix-fast-build -- "$@"
  else
    nix run github:mic92/nix-fast-build -- "$@"
  fi
}

flakify() {
  if [ ! -e flake.nix ]; then
    nix flake new -t github:Mic92/flake-templates#nix-develop .
  elif [ ! -e .envrc ]; then
    echo "use flake" > .envrc
  fi
  direnv allow
  ${EDITOR:-vim} flake.nix
}

open() {
  if [[ -n "${commands[xdg-open]}" ]]; then
    xdg-open "$@"
  elif [[ -n "${commands[kde-open5]}" ]]; then
    kde-open5 "$@"
  elif [[ -n "${commands[gnome-open]}" ]]; then
    gnome-open "$@"
  elif [[ -n "${commands[open]}" ]]; then
    open "$@"
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
function faketty { script -qfc "$(printf "%q " "$@")"; }

tmux-upterm() {
  if [ -z "$1" ]; then
    echo "Usage: tmux-upterm <github-username>"
    return 1
  fi
  upterm host --github-user "$1" --server ssh://upterm.thalheim.io:2323 \
    --force-command 'tmux attach -t pair-programming' \
    -- tmux new -t pair-programming
}

## Autocycle
setopt autopushd

# Plugins
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

if [[ "$TERM" == "linux" ]]; then # solarized-light
  echo -en "\e]PB839496" # S_base00
  echo -en "\e]PA93a1a1" # S_base01
  echo -en "\e]P0eee8d5" # S_base02
  echo -en "\e]P62aa198" # S_cyan
  echo -en "\e]P8fdf6e3" # S_base03
  echo -en "\e]P2859900" # S_green
  echo -en "\e]P5d33682" # S_magenta
  echo -en "\e]P1dc322f" # S_red
  echo -en "\e]PC657b83" # S_base0
  echo -en "\e]PE586e75" # S_base1
  echo -en "\e]P9cb4b16" # S_orange
  echo -en "\e]P7073642" # S_base2
  echo -en "\e]P4268bd2" # S_blue
  echo -en "\e]P3b58900" # S_yellow
  echo -en "\e]PF002b36" # S_base3
  echo -en "\e]PD6c71c4" # S_violet
  clear # against bg artifacts
fi

if [ -n "${commands[r2]}" ]; then
  r2() {
    if [[ "$#" -eq 0 ]]; then
      command r2 -
    else
      command r2 "$@"
    fi
  }
fi


if [[ $commands[kubectl] ]]; then
   alias k=kubectl
   source <(kubectl completion zsh)
fi
alias tf=terraform
alias tg=terragrunt

if [[ -n "${commands[fzf-share]}" ]]; then
  FZF_CTRL_R_OPTS=--reverse
  if [[ -n "${commands[fd]}" ]]; then
    export FZF_DEFAULT_COMMAND='fd --type f'
  fi
  source "$(fzf-share)/key-bindings.zsh"
fi
if [[ -f ~/.fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ]]; then
  source ~/.fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fi

if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
  source "$GHOSTTY_RESOURCES_DIR"/shell-integration/zsh/ghostty-integration
fi

if [[ -n "${commands[atuin]}" ]]; then
  export ATUIN_NOBIND="true"
  eval "$(atuin init zsh)"

  bindkey '^r' _atuin_search_widget

  # depends on terminal mode
  bindkey '^[[A' _atuin_search_widget
  bindkey '^[OA' _atuin_search_widget
fi

# prevent broken terminals by resetting to sane defaults after a command
ttyctl -f
