#!/usr/bin/env zsh
# direnv-instant.zsh - Non-blocking direnv integration for zsh
# Provides instant prompts by running direnv asynchronously in the background

# Global state variables
typeset -g __DIRENV_INSTANT_MONITOR_PID=""
typeset -g __DIRENV_INSTANT_ENV_FILE=""
typeset -g __DIRENV_INSTANT_ENV_MTIME=""
typeset -g __DIRENV_INSTANT_CURRENT_DIR=""
typeset -g __DIRENV_INSTANT_PID_FILE=""

# Get file modification time using zsh builtin stat
_direnv_get_mtime() {
    local _file="$1"
    zstat +mtime "$_file" 2>/dev/null
}

_direnv_has_route() {
    case "$OSTYPE" in
        darwin*)
            command netstat -nr 2>/dev/null | command grep -q '^default'
            ;;
        *bsd*)
            command netstat -rn 2>/dev/null | command grep -q '^default'
            ;;
        *)
            # Linux: try ip command first, fall back to route
            if command -v ip >/dev/null 2>&1; then
                command ip -4 route show default 2>/dev/null | command grep -q '^default' || \
                command ip -6 route show default 2>/dev/null | command grep -q '^default'
            else
                command route -n 2>/dev/null | command grep -q '^0\.0\.0\.0'
            fi
            ;;
    esac
}

_direnv_can_reach_internet() {
    command -v ping >/dev/null 2>&1 || return 1
    
    # Try IPv4 first (9.9.9.9 - Quad9 DNS)
    command ping -c 1 -W 1 9.9.9.9 >/dev/null 2>&1 && return 0
    
    # Try IPv6 (2620:fe::fe - Quad9 DNS)
    command ping6 -c 1 -W 1 2620:fe::fe >/dev/null 2>&1 && return 0
    
    # Some systems use unified ping command for IPv6
    command ping -c 1 -W 1 2620:fe::fe >/dev/null 2>&1 && return 0
    
    return 1
}

# Check if we have working network connectivity
_direnv_has_network() {
    _direnv_has_route && _direnv_can_reach_internet
}

# Kill monitor process only
_direnv_kill_monitor() {
    # Read PID from file if available
    local _pid=""
    if [[ -f "$__DIRENV_INSTANT_PID_FILE" ]]; then
        _pid=$(<"$__DIRENV_INSTANT_PID_FILE" 2>/dev/null)
        command rm -f "$__DIRENV_INSTANT_PID_FILE" 2>/dev/null
    fi
    
    # Use PID from file or fall back to variable
    [[ -z "$_pid" ]] && _pid="$__DIRENV_INSTANT_MONITOR_PID"
    
    if [[ -n "$_pid" ]]; then
        # Kill just the process, not the process group
        command kill "$_pid" 2>/dev/null
        __DIRENV_INSTANT_MONITOR_PID=""
    fi
    
    __DIRENV_INSTANT_CURRENT_DIR=""
}

# Load environment from file if it has changed
_direnv_load_env() {
    [[ -z "$__DIRENV_INSTANT_ENV_FILE" ]] || [[ ! -f "$__DIRENV_INSTANT_ENV_FILE" ]] && return 0
    
    local _current_mtime=$(_direnv_get_mtime "$__DIRENV_INSTANT_ENV_FILE")
    if [[ "$_current_mtime" != "$__DIRENV_INSTANT_ENV_MTIME" ]]; then
        __DIRENV_INSTANT_ENV_MTIME="$_current_mtime"
        eval "$(<"$__DIRENV_INSTANT_ENV_FILE")" 2>/dev/null || true
    fi
}

# Background process that manages direnv execution and tmux pane
_direnv_tmux_manager() {
    local _env_file="$1"
    local _parent_pid="$2"
    local _pwd="$3"
    local _direnv_cmd="$4"
    local _pid_file="$5"
    local _pane=""
    local _temp_file="${_env_file}.tmp.$$"
    
    # Write our PID to file
    echo $$ > "$_pid_file"
    
    # Cleanup function for resources
    _direnv_cleanup() {
        # Clean up tmux pane
        if [[ -n "$_pane" ]] && command tmux list-panes -a -F "#{pane_id}" 2>/dev/null | command grep -q "^${_pane}$"; then
            command tmux kill-pane -t "$_pane" 2>/dev/null || true
        fi
        # Clean up temp files
        [[ -f "$_temp_file" ]] && command rm -f "$_temp_file" 2>/dev/null || true
        [[ -f "${_temp_file}.diff" ]] && command rm -f "${_temp_file}.diff" 2>/dev/null || true
        # Clean up PID file
        [[ -f "$_pid_file" ]] && command rm -f "$_pid_file" 2>/dev/null || true
    }
    
    # Set up trap to clean up on exit
    trap '_direnv_cleanup' EXIT INT TERM
    
    # Choose execution strategy based on environment
    if ! _direnv_has_network; then
        return 0
    elif [[ -n "$TMUX" ]]; then
        # Save DIRENV_DIFF to a file if it exists
        local _diff_file="${_temp_file}.diff"
        if [[ -n "$DIRENV_DIFF" ]]; then
            echo "$DIRENV_DIFF" > "$_diff_file"
        fi
        
        # In tmux with network: create visible pane
        _pane=$(command tmux split-window -d -P -F "#{pane_id}" -l 10 \
            "cd '$_pwd' && \
             [[ -f '$_diff_file' ]] && export DIRENV_DIFF=\"\$(< '$_diff_file')\" && \
             command rm -f '$_diff_file' && \
             $_direnv_cmd export zsh > '$_temp_file' && \
             result=\$? && \
             if [ \$result -eq 0 ]; then \
                 command mv -f '$_temp_file' '$_env_file'; \
                 command sleep 5; \
             else \
                 echo 'direnv failed with exit code:' \$result; \
                 read -k1 '?Press any key to close...'; \
             fi; \
             ")
        
        # Wait for pane to close
        while command tmux list-panes -a -F "#{pane_id}" 2>/dev/null | command grep -q "^${_pane}$"; do
            command sleep 0.5
        done
    else
        # Not in tmux: run silently only if we have network
        if _direnv_has_network; then
            cd "$_pwd" && $_direnv_cmd export zsh > "$_temp_file" 2>&1 && command mv -f "$_temp_file" "$_env_file"
        fi
    fi
    
    # Signal parent that environment is ready
    command kill -USR1 $_parent_pid 2>/dev/null || true
}

# SIGUSR1 handler - loads environment when signaled by background process
_direnv_handler() {
    _direnv_load_env
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
    
    _direnv_load_env
    
    # Check if we're in a directory with .envrc
    local _envrc_dir=$(_direnv_find_envrc)
    
    if [[ -z "$_envrc_dir" ]]; then
        # No .envrc: clean up and unload
        _direnv_kill_monitor
        __DIRENV_INSTANT_ENV_FILE=""
        __DIRENV_INSTANT_ENV_MTIME=""
        __DIRENV_INSTANT_PID_FILE=""
        eval "$(${commands[direnv]} export zsh)" 2>/dev/null
        return 0
    fi
    
    # Check if we need to restart monitor (different directory)
    if [[ "$_envrc_dir" != "$__DIRENV_INSTANT_CURRENT_DIR" ]]; then
        _direnv_kill_monitor
        __DIRENV_INSTANT_ENV_FILE=""
        __DIRENV_INSTANT_ENV_MTIME=""
        __DIRENV_INSTANT_PID_FILE=""
    fi
    
    # Set up environment file path if not set
    if [[ -z "$__DIRENV_INSTANT_ENV_FILE" ]]; then
        local _direnv_dir="$_envrc_dir/.direnv"
        [[ -d "$_direnv_dir" ]] || command mkdir -p "$_direnv_dir"
        __DIRENV_INSTANT_ENV_FILE="$_direnv_dir/instant-export"
        __DIRENV_INSTANT_PID_FILE="$_direnv_dir/instant-pid"
    fi
    
    # Check if we already have a valid environment file
    if [[ -f "$__DIRENV_INSTANT_ENV_FILE" ]] && [[ -n "$__DIRENV_INSTANT_ENV_MTIME" ]]; then
        # Environment already loaded, no need to start monitor
        return 0
    fi
    
    # Check if monitor is already running via PID file
    if [[ -f "$__DIRENV_INSTANT_PID_FILE" ]]; then
        local _existing_pid=$(<"$__DIRENV_INSTANT_PID_FILE" 2>/dev/null)
        if [[ -n "$_existing_pid" ]] && command kill -0 "$_existing_pid" 2>/dev/null; then
            # Monitor is still running
            return 0
        else
            # Stale PID file, clean it up
            command rm -f "$__DIRENV_INSTANT_PID_FILE" 2>/dev/null
        fi
    fi
    
    # Start new monitor
    __DIRENV_INSTANT_CURRENT_DIR="$_envrc_dir"
    
    # Launch background process
    (
        _direnv_tmux_manager "$__DIRENV_INSTANT_ENV_FILE" $$ "$PWD" "${commands[direnv]}" "$__DIRENV_INSTANT_PID_FILE"
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
    
    # Load zsh stat module for file modification time
    zmodload -F zsh/stat b:zstat
    
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
