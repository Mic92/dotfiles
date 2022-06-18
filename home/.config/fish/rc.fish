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

# Run in background
function r; eval "$argv &" 2>&1 >/dev/null; end

function tmpdir;
  set -l dir (mktemp -d -t "$template.XXXX")
  cd $dir
end
function ff; find . -iname "*$argv*"; end
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

if is_command python;
  function pretty_json; python -mjson.tool; end
  function pretty_curl; curl -sS $args | pretty_json; end
end

# Misc
alias rot13 'tr A-Za-z N-ZA-Mn-za-m'
# diff format like git
alias diff 'diff -Naur --strip-trailing-cr'
alias todotxt "vim ~/Dropbox/todo/todo.txt"

function md5; echo -n $1 | openssl md5 /dev/stdin; end
function sha1; echo -n $1 | openssl sha1 /dev/stdin; end
function sha256; echo -n $1 | openssl dgst -sha256 /dev/stdin; end
function sha512; echo -n $1 | openssl dgst -sha512 /dev/stdin; end

if is_command direnv;
  eval (direnv hook fish)
end

function _current_epoch
  echo (math (date +%s) / 60 / 60 / 24)
end
