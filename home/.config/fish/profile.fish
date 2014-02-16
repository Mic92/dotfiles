# source environment from /etc/profile.d/* once by abusing sh
if not set -q _HAS_SOURCED_ENVIRONMENT
  eval (sh -c "source /etc/profile; env" | grep -Ev '^(PWD|SHLVL|HOME|_)' |sed 's/^\([^=]\+\)=\(.*\)/set -x \1 \'\2\';/')
end
set -x _HAS_SOURCED_ENVIRONMENT 1

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

set -x CDPATH . ~/git ./.direnv/ruby-2.0.0/gems
set -x BUNDLEPATH ~/.bundle

set -x BROWSER firefox
set -x TERMINAL urxvt
set -x PICTUREVIEW eog
set -x EDITOR vim
set -x VISUAL $EDITOR
set -x ALTERNATE_EDITOR emacs
set -x PAGER less
set -x ACK_PAGER $PAGER
set -x READNULLCMD $PAGER
set -x GREP_OPTIONS "--binary-files=without-match --directories=skip --color=auto"
set -x MANWIDTH 80
# locales
set -x LANG de_DE.UTF-8
set -x LC_ALL $LANG
# less
set -x LESS_TERMCAP_mb \e\[01\;31m      # begin blinking
set -x LESS_TERMCAP_me \e\[0m           # end mode
set -x LESS_TERMCAP_se \e\[0m           # end standout-mode
set -x LESS_TERMCAP_so \e\[01\;44\;33m  # begin standout-mode - info box
set -x LESS_TERMCAP_ue \e\[0m           # end underline
set -x LESS_TERMCAP_us \e\[03\;33\;146m # begin underline is now yellow, italic

# X11, Sound, Graphic
set -x XDG_CACHE_HOME ~/.cache
set -x XDG_CONFIG_HOME ~/.config
set -x XDG_DATA_HOME ~/.data
set -x XDG_CURRENT_DESKTOP LXDE
set -x ERRFILE ~/.xsession-errors
# Antialising
set -x QT_XFT 1
set -x GDK_USE_XFT 1
# To enable Graphic Hardware acceleration
#set LIBGL_ALWAYS_INDIRECT 1
set -x INTEL_BATCH 1
# Enable Pulse for SDL
set -x SDL_AUDIODRIVER pulse
# fix broken xdg-open
set -x GDMSESSION 1
set -x GNOME_DESKTOP_SESSION_ID 1

# make OpenJDK working with awesome wm
set -x _JAVA_AWT_WM_NONREPARENTING 1
set -x JAVA_FONTS /usr/share/fonts/TTF
