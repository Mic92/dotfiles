set -x PATH ~/bin ~/.cabal/bin /home/joerg/.npm-packages/bin /usr/bin/core_perl/ /usr/local/bin $PATH

function _clean_up_path
  set -l new_path
  for p in $PATH
    if not contains $p $new_path;
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
set -x PAGER less
set -x ACK_PAGER $PAGER
set -x READNULLCMD $PAGER
set -x GREP_OPTIONS "--binary-files=without-match --directories=skip --color=auto"
set -x MANWIDTH 80
# locales
#set -x LANG de_DE.UTF-8
#set -x LC_ALL $LANG
# less
set -x LESS "R"
set -x LESS_TERMCAP_mb \e\[01\;31m      # begin blinking
set -x LESS_TERMCAP_me \e\[0m           # end mode
set -x LESS_TERMCAP_se \e\[0m           # end standout-mode
set -x LESS_TERMCAP_so \e\[01\;44\;33m  # begin standout-mode - info box
set -x LESS_TERMCAP_ue \e\[0m           # end underline
set -x LESS_TERMCAP_us \e\[03\;33\;146m # begin underline is now yellow, italic
