#!/usr/bin/env zsh
# direnv-instant.zsh - Non-blocking direnv integration for zsh
# Provides instant prompts by running direnv asynchronously in the background

# Global state variables
typeset -g __DIRENV_INSTANT_CURRENT_DIR=""
typeset -g __DIRENV_INSTANT_ENV_FILE=""
typeset -g __DIRENV_INSTANT_STDERR_FILE=""
typeset -g __DIRENV_INSTANT_MONITOR_PID=""


# Kill monitor process only
_direnv_kill_monitor() {
    if [[ -n "$__DIRENV_INSTANT_MONITOR_PID" ]]; then
        # Kill just the process, not the process group
        command kill "$__DIRENV_INSTANT_MONITOR_PID" 2>/dev/null
        # Wait for the process to exit
        wait "$__DIRENV_INSTANT_MONITOR_PID" 2>/dev/null
        __DIRENV_INSTANT_MONITOR_PID=""
    fi

    __DIRENV_INSTANT_CURRENT_DIR=""

    # Clean up any pending files
    [[ -n "$__DIRENV_INSTANT_ENV_FILE" ]] && command rm -f "$__DIRENV_INSTANT_ENV_FILE" 2>/dev/null
    if [[ -n "$__DIRENV_INSTANT_STDERR_FILE" ]]; then
        local _stderr_file="$__DIRENV_INSTANT_STDERR_FILE"
        __DIRENV_INSTANT_STDERR_FILE=""
        command rm -f "$_stderr_file" 2>/dev/null
    fi
    __DIRENV_INSTANT_ENV_FILE=""
}


# Background process that manages direnv execution and tmux pane
_direnv_tmux_manager() {
    local _parent_pid="$1"
    local _direnv_cmd="$2"
    local _envrc_dir="$3"
    local _env_file="$4"
    local _stderr_file="$5"
    local _temp_file="$_envrc_dir/.direnv/env.$_parent_pid.tmp"

    # Clean up on exit
    trap "command rm -rf '$_temp_file' 2>/dev/null" EXIT INT TERM

    # Run direnv with script to capture all output
    # stdout goes to _temp_file, stderr is captured by script (not shown directly)
    if [[ "$OSTYPE" == darwin* ]]; then
        # macOS: run direnv directly without script due to TTY requirements
        ( $_direnv_cmd export zsh > "$_temp_file" 2> "$_stderr_file" ) &
    else
        # Linux: use script with -f flag for flush
        script -qfc "$_direnv_cmd export zsh > $_temp_file" /dev/null > "$_stderr_file" </dev/null &
    fi
    local script_pid=$!

    # Start monitoring for stderr output in background
    (
        sleep 4
        # Create tmux pane to show loading messages
        exec tmux split-window -d -l 10 zsh -c "
            command tail -f '$_stderr_file' &
            tail_pid=\$!

            # Wait for direnv to complete
            while [[ -f '$_temp_file' ]]; do
                sleep 0.1
            done

            command kill \$tail_pid 2>/dev/null
        "
    ) &

    # Wait for direnv to finish
    wait $script_pid
    local result=$?

    tmux_pid=$!
    kill $tmux_pid 2>/dev/null

    # Move files to their final locations
    if [[ $result -eq 0 && -f "$_temp_file" ]]; then
        command mv -f "$_temp_file" "$_env_file"
        kill -USR1 "$_parent_pid" 2>/dev/null
    fi
}

# SIGUSR1 handler - loads environment when signaled by background process
_direnv_handler() {
    # Display stderr output if available
    if [[ -n "$__DIRENV_INSTANT_STDERR_FILE" ]] && [[ -f "$__DIRENV_INSTANT_STDERR_FILE" ]]; then
        if [[ -s "$__DIRENV_INSTANT_STDERR_FILE" ]]; then
            echo  "$(< $__DIRENV_INSTANT_STDERR_FILE)"
        fi
        command rm -f "$__DIRENV_INSTANT_STDERR_FILE" 2>/dev/null || true
        __DIRENV_INSTANT_STDERR_FILE=""
    fi

    # Load environment variables
    if [[ -n "$__DIRENV_INSTANT_ENV_FILE" ]] && [[ -f "$__DIRENV_INSTANT_ENV_FILE" ]]; then
        eval "$(<"$__DIRENV_INSTANT_ENV_FILE")"
        command rm -f "$__DIRENV_INSTANT_ENV_FILE"
        __DIRENV_INSTANT_ENV_FILE=""
    fi
}

# Find closest parent directory containing .envrc
_direnv_find_envrc() {
    local _dir="$PWD"
    while [[ "$_dir" != "/" ]]; do
        [[ -f "$_dir/.envrc" ]] && echo "$_dir" && return 0
        _dir="${_dir:h}"
    done
    return 1
}

# Main hook called on directory changes and prompts
_direnv_hook() {
    [[ -z "${commands[direnv]}" ]] && return 0

    # For non-tmux environments, just use direnv synchronously
    if [[ -z "$TMUX" ]]; then
        eval "$(${commands[direnv]} export zsh)"
        return 0
    fi

    # Check if we're in a directory with .envrc
    local _envrc_dir=$(_direnv_find_envrc)

    if [[ -z "$_envrc_dir" ]]; then
        # No .envrc: clean up and unload
        _direnv_kill_monitor
        eval "$(${commands[direnv]} export zsh)" 2>/dev/null
        return 0
    fi

    # Check if we need to restart monitor (different directory)
    if [[ "$_envrc_dir" != "$__DIRENV_INSTANT_CURRENT_DIR" ]]; then
        _direnv_kill_monitor
    fi

    # Set up paths
    local _direnv_dir="$_envrc_dir/.direnv"
    [[ -d "$_direnv_dir" ]] || command mkdir -p "$_direnv_dir"
    local _env_file="$_direnv_dir/env.$$"

    # Check if monitor is already running
    if [[ -n "$__DIRENV_INSTANT_MONITOR_PID" ]] && command kill -0 "$__DIRENV_INSTANT_MONITOR_PID" 2>/dev/null; then
        return 0
    fi

    # Start new monitor
    __DIRENV_INSTANT_CURRENT_DIR="$_envrc_dir"
    __DIRENV_INSTANT_ENV_FILE="$_env_file"
    __DIRENV_INSTANT_STDERR_FILE="$_env_file.stderr"

    parent_pid=$$

    # Launch background process
    (
        _direnv_tmux_manager "$parent_pid" "${commands[direnv]}" "$_envrc_dir" "$__DIRENV_INSTANT_ENV_FILE" "$__DIRENV_INSTANT_STDERR_FILE"
    ) &|

    __DIRENV_INSTANT_MONITOR_PID=$!
}

# Stop direnv monitoring
direnv_instant_stop() {
    _direnv_kill_monitor
}

# Cleanup on shell exit
_direnv_exit_cleanup() {
    _direnv_kill_monitor
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
