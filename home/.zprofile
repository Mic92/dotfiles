#!/usr/bin/env zsh

# Customize to your needs...
export PATH=$PATH
path=(
    $HOME/bin
    $HOME/.cabal/bin
    $path
)
fpath=(~/.zsh $fpath)
# get rid of duplicate
typeset -U path
typeset -U fpath

# remove non-existing entries from path
path=($^path(N))
export PATH

cdpath=( ~/git )

# {{{ Prefered programs
export BROWSER=chromium
export TERMINAL=urxvt
export PICTUREVIEW=eog
export EDITOR=vim
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=vim
export PAGER=less
export ACK_PAGER=$PAGER
export READNULLCMD=$PAGER
export pacman_program=pacman-color
# }}}

# {{{ History
HISTFILE=~/.zsh_history
HISTSIZE=99999
SAVEHIST=99999
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

# make OpenJDK working with awesome wm
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

# Enforce correct locales from the beginning:
# # LC_ALL is unset since it overwrites everything
# # LANG=de_DE.UTF-8 is used, except for:
# # LC_MESSAGES=en_DK.UTF-8 never translates program output
# # LC_TIME=en_DK.UTF-8 leads to yyyy-mm-dd hh:mm date/time output
#
unset LC_ALL
export LANG=de_DE.UTF-8
export LC_MESSAGES=en_DK.UTF-8
export LC_TIME=en_DK.UTF-8

export PERL_CPANM_OPT="--local-lib=~/.perl5"
export PERL5LIB=~/.perl5/lib/perl5

# Python
export PYTHONDOCS=/usr/share/doc/python/html/
