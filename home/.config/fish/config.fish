# fish behaves very weirdly in a linux console
if test "$TERM" = linux
    exec zsh; or exec bash
# early, fast invocation of tmux
# - only if tmux is installed
# - no nested tmux sessions
else if type -q tmux; and test -z "$TMUX"
    tmux attach-session; or tmux
end

set OSTYPE (uname)
set UID (id -u)
if test -e $HOME/.nix-profile/etc/profile.d/nix-daemon.fish
    source $HOME/.nix-profile/etc/profile.d/nix-daemon.fish
end


function upfind
    set -l previous ""
    set -l current $PWD

    if test (count $argv) -ne 1
        echo "$argv[0] FILE_NAME"
        return 1
    end

    while test -d "$current" -a "$current" != "$previous"
        set -l target_path "$current/$argv[1]"
        if test -f "$target_path"
            echo "$target_path"
            return 0
        else
            set previous $current
            set current (dirname $current)
        end
    end
    return 1
end

function clone
    if test (count $argv) -eq 0
        echo "clone <GIT_CLONE_URL>"
        return 1
    end

    cd (mktemp -d) || return 1
    git clone --depth=1 "$argv[1]"
    _cd *
end

function ninja
    set -l build_path (upfind "build.ninja")
    if test -n "$build_path"
       set -l build_path (dirname build_path)
    end
    command nice -n19 ninja -C (string length -- $build_path > /dev/null; and echo $build_path; or echo ".") $argv
end

function make
    set -l build_path (upfind "Makefile")
    if test -z "$build_path"
        set -l build_path (dirname build_path)
    end
    command nice -n19 make -C (string length -- $build_path > /dev/null; and echo $build_path; or echo ".") -j(nproc) $argv
end

function real-which
    readlink -f (command which $argv)
end

function term-notify -a title message
   # requires this in tmux.conf: set allow-passthrough on
   if test $TMUX
      printf "\x1bPtmux;\x1b\x1b]777;notify;%s;%s\a\x1b\\" $title $message
    else
      printf "\x1b]777;notify;%s;%s\a" $title $message
    end
end

function command_ended --on-event fish_postexec
    set -l duration $CMD_DURATION
    if not test $duration; or test $duration -le 5000
        return
    end
    echo -e "\a" # bell sound -> will be urgent notification
end

function copypath
    if test (count $argv) -eq 0
        set p $PWD
    else
        set p $argv[1]
    end
    set p (realpath "$p")
    if test -n "$WAYLAND_DISPLAY"
        echo "$p" | wl-copy 2>/dev/null
    else if test -n "$DISPLAY"
        echo "$p" | xclip -selection clipboard 2>/dev/null
    end
    echo "$p"
end

function tempdir
    set random_adjective (shuf -n 1 $HOME/.zsh/random-adjective.txt)
    set random_name (shuf -n 1 $HOME/.zsh/random-name.txt)

    # Create a temporary directory with a random name
    cd (mktemp -d "/tmp/$random_adjective-$random_name-XXXXXX")
end

function fd
    if type -q fd
        command fd $argv
    else
        find . -iname "*$argv[0]*" 2>/dev/null
    end
end
function own
    if type -q sudo
        sudo chown -R $USER:(id -gn) $argv
    else
        chown -R "$USER:$(id -gn)" $argv
    end
end
function nixify
    set -q EDITOR; or set -l EDITOR vim
    if not test -e shell.nix -a -e default.nix
        nix flake new -t github:Mic92/flake-templates#nix-shell .
    else if not test -e ./.envrc
        echo "use nix" >.envrc
    end
    direnv allow
    "$EDITOR" default.nix
end
function nix-update
    if test -e "$HOME/git/nix-update/flake.nix"
        nix run "$HOME/git/nix-update#nix-update" -- $argv
    else
        nix run nixpkgs#nix-update -- $argv
    end
end
function nix-fast-build
    if test -e "$HOME/git/nix-fast-build/flake.nix"
        nix run "$HOME/git/nix-fast-build#nix-ci-build" -- $argv
    else
        nix run github:mic92/nix-fast-build -- $argv
    end
end
function flakify
    set -q EDITOR; or set -l EDITOR vim
    if not test -e flake.nix
        nix flake new -t github:Mic92/flake-templates#nix-develop .
    else if not test -e .envrc
        echo "use flake" >.envrc
    end
    direnv allow
    "$EDITOR" default.nix
end
function mkcd
    mkdir -p "$argv[1]"; and cd "$argv[1]"
end
if type -q zoxide
    zoxide init fish | source
end
functions --copy cd _cd
function cd
    if test "$argv[1]" = --
        set argv $argv[2..-1]
    else if test "$argv[1]" = -
        _cd -
        return
    end

    if test (count $argv) -eq 0
        _cd
        return
    end
    set -l to $argv[1]

    if test -f "$to"
        set to (dirname $to)
    end

    ## fallback to zoxide if builtin cd fails
    if not _cd "$to"
        if type -q zoxide
            __zoxide_z $to
        end
    end
end
function untilport
    if test (count $argv) -lt 2
        echo "$argv[0]: host port"
        return 1
    end
    while not nc -z $argv
        sleep 1
    end
end
# enable side-by-side diff if terminal is wide enough
function my_signal_handler --on-signal WINCH
    if test $COLUMNS -ge 140
        set -x DELTA_FEATURES side-by-side
    else
        set -x DELTA_FEATURES ''
    end
end
function unlock_root
    set -l pw (rbw get 'zfs encryption')
    ssh root@eve.i -p 2222 "echo $pw | systemd-tty-ask-password-agent"
end
if string match --quiet "linux*" "$OSTYPE"
    function ss
        # -p requires sudo to see all processes
        if echo $argv | grep -q p
            then
            sudo ss $argv | tee
        else
            command ss $argv | tee
        end
    end
end
if type -q atuin
    atuin init fish | source
end
function wttr
    if count $argv -gt 0
        set city $argv[1]
    end
    curl --compressed "wttr.in/$city"
end
function r2
    if test (count $argv) -eq 0
        command r2 -
    else
        command r2 $argv
    end
end

# The latest shell will mirror these env vars into all other shells. This is useful for ssh agent forwarding
for var in SSH_AUTH_SOCK SSH_CONNECTION SSH_CLIENT
    if set -q $var
        set -Ux $var
    end
end

function kpaste
    set -l arg cat
    if test (count $argv) -ne 0
        set -a arg $argv
    else if test -t 0; and status --is-interactive
        set arg wl-paste
    end
    command $arg | curl -sS http://p.r --data-binary @- | sed '$ {p;s|http://p.r|https://p.krebsco.de|}'
end

function hm
    nix run "$HOME/.homesick/repos/dotfiles#hm" -- $argv
end

function merge-after-ci
    echo "use merge-when-green instead" >&2
    return 1
end

function passgen
    set -l pass (nix run nixpkgs#xkcdpass -- -d '-' -n 3 -C capitalize $argv)
    echo "$pass$(random 1 10)"
end


# Filemanagement
alias free "free -g"
alias fuser "fuser -v"
alias du "du -hc"
alias df "df -hT"
# File management
if type -q lsd
    if type -q vivid
        set -x LS_COLORS (vivid generate solarized-light)
    end
    alias ls "lsd --classify --date=relative"
else if string match --quiet "freebsd*" "$OSTYPE"; or string match --quiet "darwin*" "$OSTYPE"
    alias ls 'ls -G'
else
    alias ls 'ls --color=auto --classify --human-readable'
end
abbr -a sl ls

# Basic commands
alias zcat 'zcat -f'
alias dd 'dd status=progress'
function rg
  set -l pager $PAGER

  if type -q delta
    set pager delta
  end

  if type -q rg
    begin
      command rg --sort path --smart-case --fixed-strings --json -C 2 $argv
      if test -t 0
        command rg --files | command rg --no-line-number --json -C 2 $argv
      end
    end | $pager
  else
    grep -r -C 2 $argv
  end
end
alias pgrep 'pgrep -a'

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
        command xcp -r "$argv[1]" "$newfilename"
    else
        command cp --reflink=auto -arv -- "$argv[1]" "$newfilename"
    end
end
function mv
    if test (count $argv) -ne 1 -o ! -e $argv[1]
        command mv -v $argv
        return
    end

    set -l newfilename
    read -p 'set_color green; echo -n "> "; set_color normal' -c "$argv[1]" newfilename
    command mv -v -- "$argv[1]" "$newfilename"
end
alias mkdir "mkdir -p"
abbr -a lg lazygit
if type -q q
    alias dig='q'
end
if type -q procs
    alias ps 'procs --theme light'
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
if type -q nom
  alias nix-build nom-build
end
function nixos-build
    if test (count $argv) -lt 1
        if test (uname) = "Linux"
            set name (cat /proc/sys/kernel/hostname)
        else
            echo "USAGE: $argv[0] name" >&2
            return 1
        end
    else
        set name $argv[1]
        set argv (count $argv) > /dev/null; and set argv $argv[2..-1]
    end

    command nixos-rebuild build --flake ".#$name" $argv
end
if type -q hub
    alias git hub
end
if type -q scc
    alias cloc scc
end
if type -q sgpt
    function sgpt
        set -l api_key (rbw get openai-api-key)
        if test -n "$api_key"
            OPENAI_API_KEY=$api_key command sgpt $argv
        else
            echo "No API key found"
        end
    end
end
if type -q fzf-share
    set -x FZF_CTRL_R_OPTS --reverse
    if type -q fd
        set -x FZF_DEFAULT_COMMAND 'fd --type f'
    end
    # defaults to 30ms which is insanely fast
    set fish_escape_delay_ms 500
    source (fzf-share)/key-bindings.fish
end

# Root
if type -q nvim
    alias vim nvim
end

# Miscellaneous
# diff format like git
alias diff 'diff -Naur --strip-trailing-cr'
abbr -a :q exit
alias grep "grep --binary-files=without-match --directories=skip --color=auto"
alias R "R --quiet"
alias strace "strace -yy"

if type -q direnv
    eval (direnv hook fish)
end
if type -q bat
    function cat
        if test -t 1; and status --is-interactive
            if test -n "$WAYLAND_DISPLAY"
                wl-copy < "$argv[0]" 2>/dev/null &
            end
            bat $argv
        else
            command cat $argv
        end
    end
end

fish_add_path ~/bin ~/.cabal/bin $HOME/.cargo/bin $HOME/go/bin /home/joerg/.npm-packages/bin /usr/local/bin

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
    if type -q firefox
        set -x BROWSER firefox
    end
else
    set -x BROWSER echo
end
set -x TERMINAL ghostty
set -x PICTUREVIEW gwenview

if type -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else if type -q vim
    set -x EDITOR vim
    set -x VISUAL vim
end
export VISUAL=$EDITOR

if type -q moar
    set -x MANPAGER moar
    set -x PAGER moar
    set -x MOAR '--no-linenumbers --quit-if-one-screen'
    alias less moar
else if type -q less
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
if type -q kubectl
    alias k=kubectl
    kubectl completion fish | source
end
alias tf=terraform
alias tg=terragrunt
if ! test -d "$GOPATH"
    mkdir -p "$GOPATH/src" 2>/dev/null
end
if test -S "/run/user/$UID/ssh-agent"
    set -x SSH_AUTH_SOCK "/run/user/$UID/ssh-agent"
end

if test -f "$HOME/.homesick/repos/homeshick/homeshick.fish"
    source "$HOME/.homesick/repos/homeshick/homeshick.fish"
end

if test -f "$HOME/.fish-pure/conf.d/pure.fish"
    set fish_function_path "$HOME/.fish-pure/functions" $fish_function_path
    source "$HOME/.fish-pure/conf.d/pure.fish"
end

if test -f "$HOME/.fish-async-prompt/conf.d/__async_prompt.fish"
    source "$HOME/.fish-async-prompt/conf.d/__async_prompt.fish"
end
set -g async_prompt_functions _pure_prompt_git
set -g _old_pwd $PWD
function _ls_after_cd --on-event fish_prompt
    if test "$_old_pwd" != "$PWD"
        set _old_pwd $PWD
        ls
    end
end
if test -d "$HOME/.fish-autopair/"
    set fish_function_path "$HOME/.fish-autopair/functions" $fish_function_path
    source "$HOME/.fish-autopair/conf.d/autopair.fish"
end
