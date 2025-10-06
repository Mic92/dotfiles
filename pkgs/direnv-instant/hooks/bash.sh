#!/usr/bin/env bash
# direnv-instant.sh - Non-blocking direnv integration for bash
# Provides instant prompts by running direnv asynchronously in the background

# Global state variables
__DIRENV_INSTANT_ENV_FILE=""
__DIRENV_INSTANT_STDERR_FILE=""

# SIGUSR1 handler - loads environment when signaled by Rust daemon
_direnv_handler() {
  # Display stderr output if available
  if [[ -n $__DIRENV_INSTANT_STDERR_FILE ]] && [[ -f $__DIRENV_INSTANT_STDERR_FILE ]]; then
    if [[ -s $__DIRENV_INSTANT_STDERR_FILE ]]; then
      cat "$__DIRENV_INSTANT_STDERR_FILE"
    fi
    rm -f "$__DIRENV_INSTANT_STDERR_FILE" 2>/dev/null || true
    __DIRENV_INSTANT_STDERR_FILE=""
  fi

  # Load environment variables
  if [[ -n $__DIRENV_INSTANT_ENV_FILE ]] && [[ -f $__DIRENV_INSTANT_ENV_FILE ]]; then
    eval "$(<"$__DIRENV_INSTANT_ENV_FILE")"
    rm -f "$__DIRENV_INSTANT_ENV_FILE"
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
  __DIRENV_INSTANT_HOOKED=1

  # Set up signal handler
  trap '_direnv_handler' USR1

  # Set up exit handler
  trap '_direnv_exit_cleanup' EXIT

  # Register bash PROMPT_COMMAND hook
  if [[ -z ${PROMPT_COMMAND} ]]; then
    PROMPT_COMMAND="_direnv_hook"
  elif [[ ! ${PROMPT_COMMAND} =~ (^|;)[[:space:]]*_direnv_hook($|;) ]]; then
    PROMPT_COMMAND="_direnv_hook;${PROMPT_COMMAND}"
  fi

  # Run initial hook
  _direnv_hook
fi
