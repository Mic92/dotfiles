#
# ~/.bash_profile
#

# progs
export EDITOR=vim
export VISUAL=$EDITOR
export BROWSER=firefox
export PAGER=less
export ACK_PAGER=$PAGER
export READNULLCMD=$PAGER
export BROWSER=firefox
export TERMINAL=urxvt
export PICTUREVIEW=eog
export ALTERNATE_EDITOR=emacs
export GREP_OPTIONS='--binary-files=without-match --directories=skip --color=auto'
export pacman_program=pacman-color

# {{{ X11, Sound, Graphic
export XDG_CACHE_HOME=~/.cache
export XDG_CONFIG_HOME=~/.config
export XDG_DATA_HOME=~/.data
export ERRFILE=~/.xsession-errors
# Antialising
export QT_XFT=1
export GDK_USE_XFT=1
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# To enable Graphic Hardware acceleration
#export LIBGL_ALWAYS_INDIRECT=1
export INTEL_BATCH=1
# Enable Pulse for SDL
export SDL_AUDIODRIVER=pulse
# fix broken xdg-open
export GDMSESSION=1 GNOME_DESKTOP_SESSION_ID=1

# make OpenJDK working with awesome wm
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_FONTS=/usr/share/fonts/TTF
# }}}

# Python
export PYTHONDOCS=/usr/share/doc/python/html/

# Man
export MANWIDTH=80

# path
PATH=$HOME/bin:$HOME/.cabal/bin:$HOME/.gems/bin:$PATH
export CDPATH=.:/home/git:/var/abs/local/yaourtbuild:/var/log

[ -n $BASH -a -r ~/.bashrc ] && . ~/.bashrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
