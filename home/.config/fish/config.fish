if test -f "$HOME/.homesick/repos/homeshick/homeshick.fish"
  source "$HOME/.homesick/repos/homeshick/homeshick.fish"
end

set OSTYPE (uname)
set UID (id -u)

function is_command
    if test (count $argv) -eq 1
        test -n (command -s $argv[1])
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
    set random_adjective (shuf -n 1 $HOME/.zsh/random-adjective.txt)
    set random_name (shuf -n 1 $HOME/.zsh/random-name.txt)

    # Create a temporary directory with a random name
    cd (mktemp -d "/tmp/$random_adjective-$random_name-XXXXXX")
end
function fd
    if is_command fd
        command fd "$argv"
    else
        find . -iname "*$argv*" 2>/dev/null
    end
end
function own
    if is_command sudo
        sudo chown -R $USER:(id -gn) "$argv"
    else
        chown -R "$USER:$(id -gn)" "$argv"
    end
end
function nixify
    set -q EDITOR; or set -l EDITOR vim
    if not test -e shell.nix -a -e default.nix
        nix flake new -t github:Mic92/flake-templates#nix-shell .
    else if not test -e ./.envrc
        echo "use nix" > .envrc
    end
    direnv allow
    "$EDITOR" default.nix
end
function nix-update
    if test -e "$HOME/git/nix-update/flake.nix"
        nix run "$HOME/git/nix-update#nix-update" -- "$argv"
    else
        nix run nixpkgs#nix-update -- "$argv"
    end
end
function nix-fast-build
    if test -e "$HOME/git/nix-fast-build/flake.nix"
        nix run "$HOME/git/nix-fast-build#nix-ci-build" -- "$argv"
    else
        nix run github:mic92/nix-fast-build -- "$argv"
    end
end
function flakify
    set -q EDITOR; or set -l EDITOR vim
    if not test -e flake.nix
        nix flake new -t github:Mic92/flake-templates#nix-develop .
    else if not test -e .envrc
        echo "use flake" > .envrc
    end
    direnv allow
    "$EDITOR" default.nix
end
function mkcd
  mkdir -p "$argv[1]"; and cd "$argv[1]"
end
if is_command zoxide
  zoxide init fish | source
end
#function cd
#    if test "$argv[1]" = "--"
#        set argv $argv[2..-1]
#    end
#
#    if test (count $argv) -eq 0
#        builtin cd
#        return
#    end
#
#    if test -f "$to"
#        set to (dirname $to)
#    end
#
#    # fallback to zoxide if builtin cd fails
#    if not builtin cd "$to" 2>/dev/null
#        if type -q zoxide
#            __zoxide_z $to
#        end
#    end
#end
function unlock_root
  set -l pw (rbw get 'zfs encryption')
  ssh root@eve.i -p 2222 "echo $pw | systemd-tty-ask-password-agent"
end
if string match --quiet "linux*" "$OSTYPE"
    function ss
        # -p requires sudo to see all processes
        if echo "$argv" | grep -q "p"; then
            sudo ss "$argv" | tee
        else
            command ss "$argv" | tee
        end
    end
end


# Filemanagement
alias free "free -g"
alias fuser "fuser -v"
alias du "du -hc"
alias df "df -hT"
# File management
if is_command lsd
  if is_command vivid
    set -x LS_COLORS (vivid generate solarized-light)
  end
  alias ls "lsd --classify --date=relative"
else if string match --quiet "freebsd*" "$OSTYPE"; or string match --quiet "darwin*" "$OSTYPE"
  alias ls 'ls -G'
else
  alias ls 'ls --color=auto --classify --human-readable'
end
alias sl ls


alias rm "rm -rv"
alias cp "cp -rpv"
function cp
    if test (count $argv) -ne 1 -o ! -e $argv[1]
        if test -n (command -s xcp)
            command xcp -r $argv
        else
            command cp --reflink=auto -arv $argv
        end
        return
    end

    set -l newfilename
    read -p 'set_color green; echo -n "> "; set_color normal' -c "$argv[1]" newfilename
    if test -n (command -s xcp)
        command xcp -r $argv[1] $newfilename
    else
        command cp --reflink=auto -arv -- $argv[1] $newfilename
    end
end
function mv
    if test (count $argv) -ne 1 -o ! -e $argv[1]
        command mv -v $argv
        return
    end

    set -l newfilename
    read -p 'set_color green; echo -n "> "; set_color normal' -c "$argv[1]" newfilename
    command mv -v -- $argv[1] $newfilename
end
alias mkdir "mkdir -p"
alias lg lazygit
if is_command q
    alias dig='q'
end
if is_command procs
    alias ps procs
else
    alias ps 'ps auxf'
end
function ip
    set -l colour -c
    set -l brief -br
    if string match --quiet "darwin*" $OSTYPE
        # iproute2mac
        set colour
        set brief
    end
    if test (count $argv) -eq 0
        command ip $colour $brief a
    else
        command ip $colour $argv
    end
end


alias wget "wget --continue --show-progress --progress=bar:force:noscroll"
alias curl 'curl --compressed --proto-default https'
alias nixos-rebuild 'nixos-rebuild --use-remote-sudo'
if is_command nix
    alias nix-env 'nix-env -i'
end
if is_command hub
    alias git 'hub'
end
if is_command scc
    alias cloc=scc
end
if is_command sgpt
    function sgpt
        set -l api_key (rbw get openai-api-key)
        if test -n "$api_key"
            OPENAI_API_KEY=$api_key command sgpt $argv
        else
            echo "No API key found"
        end
    end
end
if is_command fzf-share
    set -x FZF_CTRL_R_OPTS --reverse
    if is_command fd
        set -x FZF_DEFAULT_COMMAND 'fd --type f'
    end
    # defaults to 30ms which is insanely fast
    set fish_escape_delay_ms 500
    source (fzf-share)/key-bindings.fish
end

# Root
if is_command nvim
   alias vim nvim
end

# Miscellanious
# diff format like git
alias diff 'diff -Naur --strip-trailing-cr'
alias :q exit
alias grep "grep --binary-files=without-match --directories=skip --color=auto"
alias R "R --quiet"
alias strace "strace -yy"

if is_command direnv
    eval (direnv hook fish)
end
if is_command bat
    function cat
        if test -t 1; and status --is-interactive
            if test -n "$WAYLAND_DISPLAY"
                wl-copy < "$1" 2>/dev/null &
            end
            bat "$argv"
        else
            command cat "$argv"
        end
    end
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

if test -n "$WAYLAND_DISPLAY" -o -n "$DISPLAY"
    if is_command firefox
        set -x BROWSER firefox
    end
else
    set -x BROWSER echo
end
set -x TERMINAL ghostty
set -x PICTUREVIEW gwenview

if is_command nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else if is_command vim
    set -x EDITOR vim
    set -x VISUAL vim
end
export VISUAL=$EDITOR

if is_command moar
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

set -x GOPATH "$HOME/go"
if ! test -d "$GOPATH"
    mkdir -p "$GOPATH/src" 2>/dev/null
end
if test -S "/run/user/$UID/ssh-agent"
    set -x SSH_AUTH_SOCK "/run/user/$UID/ssh-agent"
end
