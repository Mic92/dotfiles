# early, fast invocation of tmux
# - only if tmux is installed
# - not in linux ttys
# - no nested tmux sessions
if [[ -n ${commands[tmux]} && "$TERM" != "linux" && -z "$TMUX" ]]; then
  tmux attach-session || tmux
  [[ $? = "0" ]] && exit
fi

source $HOME/.zshuery/zshuery.sh
load_defaults
load_aliases
load_correction
load_completion $HOME/.zsh-completion

bindkey -e
source $HOME/.zprompt

# {{{ xalias - only supposed to be used with simple aliases.
function xalias() {
	local key val com
	if (( ${#argv} == 0 )) ; then
		printf 'xalias(): Missing argument.\n'
		return 1
	fi
	if (( ${#argv} > 1 )) ; then
		printf 'xalias(): Too many arguments %s\n' "${#argv}"
		return 1
	fi

	key="${1%%\=*}" ;  val="${1#*\=}"
	check_com ${val} && alias -- "${key}=${val}"
	return 0
}
# }}}

# {{{ xhashd - check for directory, then create hash -d
function xhashd() {
    local key val com
    if (( ${#argv} == 0 )) ; then
        printf 'xhashd(): Missing argument.\n'
        return 1
    fi
    if (( ${#argv} > 1 )) ; then
        printf 'xhashd(): Too many arguments %s\n' "${#argv}"
        return 1
    fi

    key="${1%%\=*}" ;  val="${1#*\=}"
    [[ -d ${val} ]] && hash -d -- "${key}=${val}"
    return 0
}
# }}}

# check_com - check if a command exists {{{
# eg: check_com "vim -p"
function check_com() {
    #setopt localoptions xtrace
    local -a words
    local -i comonly
    local cmd

    if [[ ${1} == '-c' ]] ; then
        (( comonly = 1 ))
        shift
    else
        (( comonly = 0 ))
    fi

    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c] <command>\n' >&2
        return 1
    fi

    words=(${(z)1})
    cmd=${words[1]}

    if (( comonly > 0 )) ; then
        [[ -n ${commands[$cmd]}  ]] && return 0
        return 1
    fi

    if   [[ -n ${commands[$cmd]}    ]] \
      || [[ -n ${functions[$cmd]}   ]] \
      || [[ -n ${aliases[$cmd]}     ]] \
      || [[ -n ${reswords[(r)$cmd]} ]] ; then

        return 0
    fi

    return 1
}
# }}}

source $HOME/.zalias
