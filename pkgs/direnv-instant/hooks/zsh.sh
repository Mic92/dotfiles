#!/usr/bin/env zsh
# shellcheck disable=SC1071
# direnv-instant.zsh - Non-blocking direnv integration for zsh
# Provides instant prompts by running direnv asynchronously in the background

# Global state variables
typeset -g __DIRENV_INSTANT_ENV_FILE=""
typeset -g __DIRENV_INSTANT_STDERR_FILE=""

# SIGUSR1 handler - loads environment when signaled by Rust daemon
_direnv_handler() {
  # Display stderr output if available
  if [[ -n $__DIRENV_INSTANT_STDERR_FILE ]] && [[ -f $__DIRENV_INSTANT_STDERR_FILE ]]; then
    if [[ -s $__DIRENV_INSTANT_STDERR_FILE ]]; then
      echo "$(<"$__DIRENV_INSTANT_STDERR_FILE")"
    fi
    command rm -f "$__DIRENV_INSTANT_STDERR_FILE" 2>/dev/null || true
    __DIRENV_INSTANT_STDERR_FILE=""
  fi

  # Load environment variables
  if [[ -n $__DIRENV_INSTANT_ENV_FILE ]] && [[ -f $__DIRENV_INSTANT_ENV_FILE ]]; then
    eval "$(<"$__DIRENV_INSTANT_ENV_FILE")"
    command rm -f "$__DIRENV_INSTANT_ENV_FILE"
    __DIRENV_INSTANT_ENV_FILE=""
  fi
}

# Main hook called on directory changes and prompts
_direnv_hook() {
  export DIRENV_INSTANT_SHELL_PID=$$
  eval "$(direnv-instant start)"
}

# Cleanup on shell exit
_direnv_exit_cleanup() {
  direnv-instant stop
}

# Initialize hooks if not already done
if [[ -z ${__DIRENV_INSTANT_HOOKED} ]]; then
  typeset -g __DIRENV_INSTANT_HOOKED=1

  # Set up signal handler
  trap '_direnv_handler' USR1

  # Register zsh hooks
  autoload -Uz add-zsh-hook
  add-zsh-hook precmd _direnv_hook
  add-zsh-hook chpwd _direnv_hook
  add-zsh-hook zshexit _direnv_exit_cleanup

  # Run initial hook
  _direnv_hook
fi
