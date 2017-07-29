set COMMANDS (find $PATH -not -type d 2>/dev/null | sed 's,.*/,,')

function is_command
  if test (count $argv) -eq 1;
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
    else if is_command tmuxinator
      tmuxinator start default; and exec true
    else
      tmux; and exec true
    end
  end
end

if is_command envoy;
  eval (envoy --agent=gpg-agent --print --fish)
  eval (envoy --agent=ssh-agent --print --fish)

  if is_command systemctl;
    systemctl --user set-enviroment SSH_AUTH_SOCK=$SSH_AUTH_SOCK SSH_AGENT_PID=$SSH_AGENT_PID GPG_AGENT_INFO=$GPG_AGENT_INFO
  end
end

function pdfag
  set f (mktemp)
  fish -c "
    pdfgrep --recursive --color always --page-number $argv[1] .
    echo -n Search finish, Enter a number to open file:
  " >$f &
  tail --follow $f
  rm -rf $f
end

# Run in background
function r; eval "$argv &" 2>&1 >/dev/null; end

# Check Internet connection
alias gping "ping -v 8.8.8.8"
alias ghost "host -v google.com 8.8.8.8"
alias gcurl "curl -v google.com"

alias eeets1 'env TERM=xterm ssh eeets1'
alias eeets2 'env TERM=xterm ssh eeets2'

function tmpdir;
  set -l template (shuf -n 1 /usr/share/dict/words 2>/dev/null)
  or set -l template (shuf -n 1 /usr/share/dict/cracklib-small 2>/dev/null)
  or set -l template "temp"
  set -l dir (mktemp -d -t "$template.XXXX")
  cd $dir
end
function ff; /usr/bin/find . -iname "*$argv*"; end
function browse; $BROWSER file://"`pwd`/$1"; end

alias ag "ag --color --smart-case --literal"
alias rag "ag --color --smart-case --path-to-agignore ~/.ruby-agignore"
alias free "free -m"
alias du "du -hc"
alias df "df -hT"
is_command dfc; and alias df dfc

# Filemanagement
alias ls "ls --color=auto -F -h"
alias ls "ls --color=auto"
alias la "ls -lA --color=auto"
alias laa "ls -la --color=auto"
alias ll "ls -l --color=auto"
alias rm "rm -rv"
alias cp "cp -rpv"
alias cpv "rsync -pogr --progress"
alias mv "mv -v"
alias mkdir "mkdir -p"
alias locate "locate --existing --follow --basename --ignore-case"

alias wget "wget -c"

# Root
alias su 'su'
alias sync 'sudo sync'
alias sync 'sudo updatedb'
alias sless 'sudo less'
alias stail 'sudo tail'
alias svim 'sudo vim'
alias svim 'sudo vimdiff'
alias vim 'vi'
# reboot/halt/suspend
alias shutdown='sudo shutdown'
alias reboot='sudo reboot'
if is_command systemctl
  alias ctl 'sudo systemctl'
  alias shutdown="systemctl poweroff"
  alias reboot="systemctl reboot"
  alias suspend="systemctl suspend"
  alias hibernate="systemctl hibernate"
  alias hybrid-sleep="systemctl hybrid-sleep"
end
if is_command ruby;
  function json_escape
    command ruby -e 'require \'json\'; puts(File.open(ARGV[0]).read.to_json) if ARGV[0]' $argv
  end
end
if is_command python;
  function pretty_json; python -mjson.tool; end
  function pretty_curl; curl -sS $args | pretty_json; end
end

# Misc
alias rot13 'tr A-Za-z N-ZA-Mn-za-m'
# diff format like git
alias diff 'diff -Naur --strip-trailing-cr'
alias todotxt "vim ~/Dropbox/todo/todo.txt"
is_command mpv; and alias mplayer mpv

# archlinux stuff
if test -f  /etc/arch-release
  alias pacman "sudo pacman"
  function owns; /usr/bin/pacman -Qo (which "$argv"); end
  function y; yaourt $argv; end
  __fish_complete_pacman y
end

function showips
  echo Current IPs: (ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')
end

function instaweb
  showips
  echo "http://0.0.0.0:4321"
  python -m http.server 4321; or python -m SimpleHTTPServer 4321
end

function instasmtp
  showips
  echo "smtp server started on port" 1025;
  python -m smtpd -n -c DebuggingServer localhost:1025
end

function md5; echo -n $1 | openssl md5 /dev/stdin; end
function sha1; echo -n $1 | openssl sha1 /dev/stdin; end
function sha256; echo -n $1 | openssl dgst -sha256 /dev/stdin; end
function sha512; echo -n $1 | openssl dgst -sha512 /dev/stdin; end

if is_command direnv;
  eval (direnv hook fish)
end

function flash_undelete
  cd /proc/(ps x | awk '/libflashplayer.so\ /{print $1}')/fd; and ls -l | grep deleted
end

function _current_epoch
  echo (math (date +%s) / 60 / 60 / 24)
end

function pacupgrade
  if is_command snapper
    sudo true
    set prenumber (sudo snapper create --type=pre --cleanup-algorithm=number --print-number --description="pacman upgrade")
    echo Create pre snapshot with number $prenumber before upgrade
    yaourt -Syu --aur
    echo Create post snapshot with number (sudo snapper create --type=post --cleanup-algorithm=number --print-number --pre-number="$prenumber")
  else
    yaourt -Syu --aur
  end
  pacman -Qu | sudo tee /var/log/pacman-updates.log >/dev/null
end

function _check_updates
  if true
    flock -n 9;
    set -l ignored_pkgs "^linux"
    #set -l updates (wc -l < /var/log/pacman-updates.log)
    set -l updates (grep -Ev $ignored_pkgs /var/log/pacman-updates.log | wc -l)
    if test $updates -gt 0
      echo "There are $updates updates. (run pacupgrade to update)"
    end
    echo (_current_epoch) > $HOME/.pacman-update
  end 9>~/.pacman-update.lck
end

# only after first update and not if pacman is running
if test -e /var/log/pacman-updates.log; and not test -e /var/lib/pacman/db.lck;
  if test -e .pacman-update
    read last_epoch < $HOME/.pacman-update
    if test -n "$last_epoch"
      if test (math (_current_epoch) - $last_epoch) -ge 1
        _check_updates
      end
    end
    set -e last_epoch
  else
    _check_updates
  end
end
