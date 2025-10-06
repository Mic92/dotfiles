[[ -z "${commands[direnv-instant]}" ]] && return 0
eval "$(direnv-instant hook zsh)"
