if test -f "$HOME/.homesick/repos/homeshick/homeshick.fish"
  source "$HOME/.homesick/repos/homeshick/homeshick.fish"
end

if status --is-login
  posix_source --sh /etc/profile
end

set COMMANDS (find $PATH -not -type d 2>/dev/null | sed 's,.*/,,')

function is_command
    if test (count $argv) -eq 1
        contains $argv[1] $COMMANDS
    else
        echo "USAGE: is_command command"
        return 1
    end
end

# early, fast invocation of tmux
# - only if tmux is installed
# - no nested tmux sessions
if is_command tmux
    if test -z "$TMUX"
        if tmux attach-session
            exec true
        else
            tmux; and exec true
        end
    end
end

# Run in background
function r
    eval "$argv &" 2>&1 >/dev/null
end

function tempdir
    set -l dir (mktemp -d -t "$template.XXXX")
    cd $dir
end
function ff
    find . -iname "*$argv*"
end
function browse
    $BROWSER file://"`pwd`/$1"
end

# Filemanagement
alias free "free -g"
alias fuser "fuser -v"
alias du "du -hc"
alias df "df -hT"
# File management
#if is_command lsd; then
#  if is_command vivid; then
#    export LS_COLORS="$(vivid generate solarized-light)"
#  fi
#  alias ls="lsd --classify --date=relative"
##elif [[ $OSTYPE == freebsd* ]] ||  [[ $OSTYPE == darwin* ]]; then
#  alias ls='ls -G'
#else
#  alias ls='ls --color=auto --classify --human-readable'
#fi
#alias sl=ls

alias rm "rm -rv"
alias cp "cp -rpv"
alias cpv "rsync -pogr --progress"
alias mv "mv -v"
alias mkdir "mkdir -p"
alias locate "locate --existing --follow --basename --ignore-case"
alias lg lazygit
if is_command q
    then
    alias dig='q'
end
if is_command procs
    then
    alias ps procs
else
    alias ps 'ps auxf'
end

alias wget "wget -c"

# Root
alias vim nvim

if is_command python
    function pretty_json
        python -mjson.tool
    end
    function pretty_curl
        curl -sS $args | pretty_json
    end
end

# Misc
alias rot13 'tr A-Za-z N-ZA-Mn-za-m'
# diff format like git
alias diff 'diff -Naur --strip-trailing-cr'
alias todotxt "vim ~/Dropbox/todo/todo.txt"

function md5
    echo -n $1 | openssl md5 /dev/stdin
end
function sha1
    echo -n $1 | openssl sha1 /dev/stdin
end
function sha256
    echo -n $1 | openssl dgst -sha256 /dev/stdin
end
function sha512
    echo -n $1 | openssl dgst -sha512 /dev/stdin
end

if is_command direnv
    eval (direnv hook fish)
end

function _current_epoch
    echo (math (date +%s) / 60 / 60 / 24)
end

set -x PATH ~/bin ~/.cabal/bin $HOME/.cargo/bin $HOME/go/bin /home/joerg/.npm-packages/bin /usr/local/bin $PATH

function _clean_up_path
    set -l new_path
    for p in $PATH
        if not contains $p $new_path
            if test -e $p
                set new_path $new_path $p
            end
        end
    end
    set -x PATH $new_path
end
_clean_up_path

set -x CDPATH . ~/git

set -x BROWSER firefox
set -x TERMINAL foot
set -x EDITOR vim
set -x VISUAL $EDITOR
set -x ALTERNATE_EDITOR emacs
set -x PAGER moar

if is_command moar
    then
    set -x MANPAGER moar
    set -x PAGER moar
    set -x MOAR '--no-linenumbers --quit-if-one-screen'
    alias less moar
else if is_command less
    set -x MANPAGER less
    set -x PAGER less
end

set -x READNULLCMD $PAGER
set -x GREP_OPTIONS "--binary-files=without-match --directories=skip --color=auto"
set -x MANWIDTH 80
# locales
set -x LC_ALL en_US.UTF-8
# less
set -x LESS -FXisRM
set -x LESS_TERMCAP_mb \e\[01\;31m # begin blinking
set -x LESS_TERMCAP_me \e\[0m # end mode
set -x LESS_TERMCAP_se \e\[0m # end standout-mode
set -x LESS_TERMCAP_so \e\[01\;44\;33m # begin standout-mode - info box
set -x LESS_TERMCAP_ue \e\[0m # end underline
set -x LESS_TERMCAP_us \e\[03\;33\;146m # begin underline is now yellow, italic

# freedesktop, Sound, Graphic
set -x XDG_DESKTOP_DIR "$HOME/Desktop"
set -x XDG_CONFIG_HOME "$HOME/.config" # for macos
set -x XDG_DOCUMENTS_DIR "$HOME/Documents"
set -x XDG_DOWNLOAD_DIR "$HOME/Downloads"
set -x XDG_MUSIC_DIR "$HOME/Music"
set -x XDG_PICTURES_DIR "$HOME/Pictures"
set -x XDG_PUBLICSHARE_DIR "$HOME/Public"
set -x XDG_TEMPLATES_DIR "$HOME/.Templates"
set -x XDG_VIDEOS_DIR "$HOME/Videos"

# Enable Pipewire for SDL
set -x SDL_AUDIODRIVER pipewire
set -x ALSOFT_DRIVERS pipewire

# downgrade terminfo to tmux if we are inside
if test -n "$TMUX" -a -n (command -v tput)
    if TERM=tmux-256color tput longname >/dev/null 2>&1
        set -x TERM tmux-256color
    else
        set -x TERM screen-256color
    end
end

source $HOME/.config/fish/solarized.fish
