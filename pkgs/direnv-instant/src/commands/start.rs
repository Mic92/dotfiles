use crate::daemon::{
    DaemonContext, direnv_export_command, get_socket_path, notify_daemon, start_daemon, stop_daemon,
};
use nix::unistd::getppid;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Stdio;

pub fn run() {
    let direnv = "direnv";
    let parent_pid = env::var("DIRENV_INSTANT_SHELL_PID")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(|| getppid().as_raw());

    // Find .envrc directory
    let envrc_dir = match find_envrc() {
        Some(dir) => dir,
        None => {
            println!("unset __DIRENV_INSTANT_CURRENT_DIR");
            run_direnv_sync(direnv, false);
            return;
        }
    };

    // Check if we need to restart daemon (different directory)
    if let Ok(current) = env::var("__DIRENV_INSTANT_CURRENT_DIR") {
        let current_dir = PathBuf::from(&current);
        if current_dir != envrc_dir {
            stop_daemon(&get_socket_path(&current_dir));
        }
    }
    export_path_var("__DIRENV_INSTANT_CURRENT_DIR", &envrc_dir);

    // If not in tmux, just run direnv synchronously
    if env::var("TMUX").is_err() {
        run_direnv_sync(direnv, true);
        return;
    }

    // Set up daemon context
    let ctx = DaemonContext::new(parent_pid, envrc_dir);
    export_path_var("__DIRENV_INSTANT_ENV_FILE", &ctx.env_file);
    export_path_var("__DIRENV_INSTANT_STDERR_FILE", &ctx.stderr_file);

    // Check if daemon is already running
    if ctx.socket_path.exists() && notify_daemon(&ctx.socket_path, parent_pid) {
        return;
    }

    start_daemon(direnv, &ctx);
}

fn find_envrc() -> Option<PathBuf> {
    let mut dir = env::current_dir().ok()?;
    loop {
        if dir.join(".envrc").exists() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn run_direnv_sync(direnv: &str, show_errors: bool) {
    let mut cmd = direnv_export_command(direnv);
    if !show_errors {
        cmd.stderr(Stdio::null());
    }
    let _ = cmd.status();
}

fn export_path_var(name: &str, path: &Path) {
    let path_str = path.display().to_string();
    let escaped = path_str.replace('\'', r"'\''");
    println!("export {}='{}'", name, escaped);
}
