#!/usr/bin/env zsh

# Customize to your needs...
export PATH=$PATH
path=(
    $HOME/bin
    $HOME/.cabal/bin
    /usr/local/{,s}bin
    /opt/local/{,s}bin
    /usr/lib/perl5/vendor_perl/bin/
    /usr/{,s}bin
    $path
)
fpath=(~/.zsh $fpath)
# get rid of duplicate
typeset -U path
typeset -U fpath

# remove non-existing entries from path
path=($^path(N))
export PATH

# {{{ Prefered programs
export BROWSER=firefox
export TERMINAL=urxvt
export PICTUREVIEW=eog
export EDITOR=vim
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=emacs
export PAGER=less
export ACK_PAGER=$PAGER
export READNULLCMD=$PAGER
export GREP_OPTIONS='--binary-files=without-match --directories=skip --color=auto'
export pacman_program=pacman-color
# }}}

# {{{ History
HISTFILE=~/.zhistory
HISTSIZE=9999
SAVEHIST=6000
DIRSTACKSIZE=30
# }}}

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

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd_hrgb -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
# make OpenJDK working with awesome wm
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_FONTS=/usr/share/fonts/TTF
# }}}

# {{{ less
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

# locales
export LANG=de_DE.UTF-8
export LC_ALL=$LANG

# Python
export PYTHONDOCS=/usr/share/doc/python/html/

