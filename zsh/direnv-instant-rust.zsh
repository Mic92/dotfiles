#!/usr/bin/env zsh
# direnv-instant-rust.zsh - Shell integration for the Rust direnv-instant binary

# SIGUSR1 handler - loads environment when signaled by Rust daemon
_direnv_handler() {
    local env_file
    local stderr_file
    
    # Find the env file based on current PID
    if [[ -n "$__DIRENV_INSTANT_DIR" ]]; then
        env_file="$__DIRENV_INSTANT_DIR/.direnv/env.$$"
        stderr_file="${env_file}.stderr"
        
        # Display stderr output if available
        if [[ -f "$stderr_file" ]] && [[ -s "$stderr_file" ]]; then
            cat "$stderr_file"
            rm -f "$stderr_file"
        fi
        
        # Load environment variables
        if [[ -f "$env_file" ]]; then
            eval "$(< "$env_file")"
            rm -f "$env_file"
        fi
    fi
}

# Main hook called on directory changes and prompts
_direnv_hook() {
    [[ -z "${commands[direnv-instant]}" ]] && return 0
    
    # Run direnv-instant and evaluate its output (only env vars)
    eval "$(${commands[direnv-instant]})"
    
    # Open FIFO for writing if we just started a daemon
    local fifo_path="${XDG_RUNTIME_DIR:-/tmp}/direnv-instant-${UID}/shell-$$.fifo"
    if [[ -p "$fifo_path" ]] && [[ -z "$__DIRENV_INSTANT_FIFO_FD" ]]; then
        # Open FIFO for writing and keep it open
        exec {__DIRENV_INSTANT_FIFO_FD}>"$fifo_path"
    fi
}

# Close FIFO FD on shell exit to signal daemon to terminate
_direnv_exit_cleanup() {
    if [[ -n "$__DIRENV_INSTANT_FIFO_FD" ]]; then
        # Close the FIFO FD - this will signal the daemon to exit
        eval "exec $__DIRENV_INSTANT_FIFO_FD>&-"
    fi
    
    # Clean up FIFO file
    local fifo_path="${XDG_RUNTIME_DIR:-/tmp}/direnv-instant-${UID}/shell-$$.fifo"
    [[ -p "$fifo_path" ]] && rm -f "$fifo_path"
}

# Initialize hooks if not already done
if [[ -z "${__DIRENV_INSTANT_HOOKED}" ]]; then
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