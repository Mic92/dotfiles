use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::os::unix::io::{AsRawFd, AsFd, BorrowedFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::net::{UnixListener, UnixStream};
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use nix::cmsg_space;
use nix::fcntl::OFlag;
use nix::pty::{OpenptyResult, openpty};
use nix::sys::signal::{self, Signal};
use nix::sys::socket::{ControlMessage, ControlMessageOwned, MsgFlags, recvmsg, sendmsg};
use nix::sys::stat::Mode;
use nix::sys::termios;
use nix::sys::wait::{WaitStatus, waitpid};
use nix::unistd::{self, Pid};

fn find_envrc_dir(start: &Path) -> Option<PathBuf> {
    let mut dir = start.to_path_buf();
    loop {
        if dir.join(".envrc").exists() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn get_runtime_dir() -> PathBuf {
    env::var("XDG_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp"))
        .join(format!("direnv-instant-{}", unistd::geteuid()))
}

fn get_fifo_path(shell_pid: u32) -> PathBuf {
    get_runtime_dir().join(format!("shell-{}.fifo", shell_pid))
}

fn get_socket_path(daemon_pid: u32) -> PathBuf {
    get_runtime_dir().join(format!("daemon-{}.sock", daemon_pid))
}

fn emit_env_var(name: &str, value: &str) {
    println!("export {}={:?};", name, value);
}

fn emit_unset_var(name: &str) {
    println!("unset {};", name);
}

fn cleanup_stale_fifos() {
    if let Ok(entries) = fs::read_dir(get_runtime_dir()) {
        for entry in entries.flatten() {
            if let Some(name) = entry.file_name().to_str() {
                if name.starts_with("shell-") && name.ends_with(".fifo") {
                    // Try to open for writing non-blocking - if it fails, no reader exists
                    let path = entry.path();
                    match nix::fcntl::open(
                        path.as_path(),
                        OFlag::O_WRONLY | OFlag::O_NONBLOCK,
                        Mode::empty(),
                    ) {
                        Ok(_fd) => {
                            // There's a reader, fd will be closed automatically when dropped
                        }
                        Err(_) => {
                            // No reader, remove stale fifo
                            let _ = fs::remove_file(&path);
                        }
                    }
                }
            }
        }
    }
}

fn run_hook() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = env::current_dir()?;
    let shell_pid = std::process::id();

    // Check if direnv exists
    let direnv_path = match Command::new("which").arg("direnv").output() {
        Ok(output) if output.status.success() => {
            String::from_utf8_lossy(&output.stdout).trim().to_string()
        }
        _ => return Ok(()), // No direnv, nothing to do
    };

    // For non-tmux environments, just run direnv synchronously
    if env::var("TMUX").is_err() {
        let output = Command::new(&direnv_path)
            .args(&["export", "zsh"])
            .output()?;

        if output.status.success() {
            print!("{}", String::from_utf8_lossy(&output.stdout));
        }
        return Ok(());
    }

    // Clean up any stale FIFOs
    cleanup_stale_fifos();

    // Find .envrc directory
    let envrc_dir = match find_envrc_dir(&cwd) {
        Some(dir) => dir,
        None => {
            // No .envrc: unload any environment
            let output = Command::new(&direnv_path)
                .args(&["export", "zsh"])
                .stderr(Stdio::null())
                .output()?;

            if output.status.success() {
                print!("{}", String::from_utf8_lossy(&output.stdout));
            }

            // Clear tracking variables
            emit_unset_var("__DIRENV_INSTANT_DIR");
            emit_unset_var("__DIRENV_INSTANT_DAEMON_PID");

            return Ok(());
        }
    };

    let envrc_dir_str = envrc_dir.to_string_lossy();

    // Check if we already have a daemon for this directory
    if let (Ok(current_dir), Ok(daemon_pid_str)) = (
        env::var("__DIRENV_INSTANT_DIR"),
        env::var("__DIRENV_INSTANT_DAEMON_PID"),
    ) {
        if current_dir == envrc_dir_str {
            // Check if daemon is still alive
            if let Ok(pid) = daemon_pid_str.parse::<i32>() {
                let daemon_pid = Pid::from_raw(pid);
                if signal::kill(daemon_pid, None).is_ok() {
                    // Daemon is still running
                    return Ok(());
                }
            }
        }
    }

    // Create FIFO for lifetime monitoring
    let fifo_path = get_fifo_path(shell_pid);
    let runtime_dir = get_runtime_dir();
    fs::create_dir_all(&runtime_dir)?;

    // Remove any existing FIFO
    let _ = fs::remove_file(&fifo_path);

    // Create new FIFO
    nix::unistd::mkfifo(&fifo_path, Mode::S_IRUSR | Mode::S_IWUSR)?;

    // Fork daemon process
    match unsafe { unistd::fork() }? {
        unistd::ForkResult::Parent { child } => {
            // Parent: export environment variables
            emit_env_var("__DIRENV_INSTANT_DIR", &envrc_dir_str);
            emit_env_var("__DIRENV_INSTANT_DAEMON_PID", &child.to_string());

            // The shell will keep the FIFO alive by having it in the environment
        }
        unistd::ForkResult::Child => {
            // Daemon process

            // Detach from parent
            let _ = unistd::setsid();

            // Redirect stdin/stdout/stderr to /dev/null
            let devnull = File::open("/dev/null")?;
            // dup2 with raw fd to avoid ownership issues
            let devnull_raw = devnull.as_raw_fd();
            unsafe {
                libc::dup2(devnull_raw, 0);
                libc::dup2(devnull_raw, 1);
                libc::dup2(devnull_raw, 2);
            }

            // Run the daemon logic with the FIFO path
            if let Err(e) = run_daemon(&direnv_path, &envrc_dir, &fifo_path, shell_pid) {
                eprintln!("Daemon error: {}", e);
            }

            // Clean up FIFO on exit
            let _ = fs::remove_file(&fifo_path);

            std::process::exit(0);
        }
    }

    Ok(())
}

fn send_fd<Fd: AsFd>(socket: &UnixStream, fd: &Fd) -> Result<(), Box<dyn std::error::Error>> {
    let msg = [0u8];
    let raw_fd = fd.as_fd().as_raw_fd();
    let fds = [raw_fd];
    let cmsg = ControlMessage::ScmRights(&fds);

    sendmsg::<()>(
        socket.as_raw_fd(),
        &[std::io::IoSlice::new(&msg)],
        &[cmsg],
        MsgFlags::empty(),
        None,
    )?;
    Ok(())
}

fn recv_fd(socket: &UnixStream) -> Result<OwnedFd, Box<dyn std::error::Error>> {
    let mut buf = [0u8; 1];
    let mut cmsg_buf = cmsg_space!([RawFd; 1]);
    let mut iov = [std::io::IoSliceMut::new(&mut buf)];

    let msg = recvmsg::<()>(
        socket.as_raw_fd(),
        &mut iov,
        Some(&mut cmsg_buf),
        MsgFlags::empty(),
    )?;

    for cmsg in msg.cmsgs()? {
        if let ControlMessageOwned::ScmRights(fds) = cmsg {
            if !fds.is_empty() {
                return Ok(unsafe { OwnedFd::from_raw_fd(fds[0]) });
            }
        }
    }

    Err("No file descriptor received".into())
}

fn run_tmux_client(daemon_pid: u32) -> Result<(), Box<dyn std::error::Error>> {
    let socket_path = get_socket_path(daemon_pid);

    // Save current terminal settings for stderr
    let stderr = std::io::stderr();
    let orig_termios = termios::tcgetattr(&stderr).ok();

    // Set stderr to raw mode if it's a terminal
    if let Some(termios) = orig_termios.as_ref() {
        let mut raw_termios = termios.clone();
        termios::cfmakeraw(&mut raw_termios);
        let _ = termios::tcsetattr(
            &stderr,
            termios::SetArg::TCSANOW,
            &raw_termios,
        );
    }

    // Connect to daemon socket
    let stream = UnixStream::connect(&socket_path)?;

    // Receive stderr FD from daemon
    let pty_fd = recv_fd(&stream)?;

    // Read from pty_fd and output to our stderr
    let mut buffer = [0u8; 4096];
    loop {
        match unistd::read(&pty_fd, &mut buffer) {
            Ok(0) => break, // EOF - daemon closed connection
            Ok(n) => {
                std::io::stderr().write_all(&buffer[..n])?;
                std::io::stderr().flush()?;
            }
            Err(nix::errno::Errno::EINTR) => continue,
            Err(_) => break,
        }
    }

    // Restore original terminal settings
    if let Some(termios) = orig_termios {
        let _ = termios::tcsetattr(
            &stderr,
            termios::SetArg::TCSANOW,
            &termios,
        );
    }

    Ok(())
}

struct DaemonPaths {
    env_file: PathBuf,
    stderr_file: PathBuf,
    temp_file: PathBuf,
    socket_path: PathBuf,
}

struct DaemonState {
    stderr_buffer: Arc<Mutex<Vec<u8>>>,
    pty_fd: Arc<Mutex<Option<OwnedFd>>>,
    tmux_connected: Arc<Mutex<bool>>,
}

fn setup_daemon_paths(
    envrc_dir: &Path,
    parent_pid: Pid,
    daemon_pid: u32,
) -> Result<DaemonPaths, Box<dyn std::error::Error>> {
    let direnv_dir = envrc_dir.join(".direnv");
    fs::create_dir_all(&direnv_dir)?;

    let env_file = direnv_dir.join(format!("env.{}", parent_pid));
    let stderr_file = env_file.with_extension("stderr");
    let temp_file = direnv_dir.join(format!("env.{}.tmp", parent_pid));
    let socket_path = get_socket_path(daemon_pid);

    // Create temp file to signal direnv is running
    File::create(&temp_file)?;

    Ok(DaemonPaths {
        env_file,
        stderr_file,
        temp_file,
        socket_path,
    })
}

fn start_lifetime_monitor(fifo_path: &Path) {
    let fifo_path_clone = fifo_path.to_path_buf();
    thread::spawn(move || {
        match nix::fcntl::open(fifo_path_clone.as_path(), OFlag::O_RDONLY, Mode::empty()) {
            Ok(fd) => {
                let mut buffer = [0u8; 1];
                loop {
                    match unistd::read(&fd, &mut buffer) {
                        Ok(0) | Err(_) => {
                            std::process::exit(0);
                        }
                        Ok(_) => continue,
                    }
                }
            }
            Err(_) => {
                std::process::exit(1);
            }
        }
    });
}

fn start_socket_listener(listener: UnixListener, state: &DaemonState, temp_file: PathBuf) {
    let stderr_buffer_clone = state.stderr_buffer.clone();
    let pty_fd_clone = state.pty_fd.clone();
    let tmux_connected_clone = state.tmux_connected.clone();

    thread::spawn(move || {
        for stream in listener.incoming() {
            if let Ok(mut stream) = stream {
                if let Ok(guard) = pty_fd_clone.lock() {
                    if let Some(ref fd) = *guard {
                        if send_fd(&stream, fd).is_ok() {
                            if let Ok(mut connected) = tmux_connected_clone.lock() {
                                *connected = true;
                            }

                            if let Ok(buffer) = stderr_buffer_clone.lock() {
                                let _ = stream.write_all(&buffer);
                            }

                            while temp_file.exists() {
                                thread::sleep(Duration::from_millis(100));
                            }
                        }
                    }
                }
                drop(stream);
            }
        }
    });
}

fn start_tmux_launcher(daemon_pid: u32, temp_file: PathBuf, daemon_start: Instant) {
    thread::spawn(move || {
        let elapsed = daemon_start.elapsed();
        if elapsed < Duration::from_secs(4) {
            thread::sleep(Duration::from_secs(4) - elapsed);
        }

        if temp_file.exists() {
            Command::new("tmux")
                .args(&[
                    "split-window",
                    "-d",
                    "-l",
                    "10",
                    &format!("direnv-instant tmux {}", daemon_pid),
                ])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
                .ok();
        }
    });
}

fn handle_direnv_output(
    master_fd: BorrowedFd,
    stderr_file_handle: &mut File,
    state: &DaemonState,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut buffer = [0u8; 4096];
    let mut all_output = Vec::new();

    loop {
        match unistd::read(master_fd, &mut buffer) {
            Ok(0) => break,
            Ok(n) => {
                let data = &buffer[..n];
                all_output.extend_from_slice(data);

                stderr_file_handle.write_all(data)?;
                stderr_file_handle.flush()?;

                let tmux_ready = state.tmux_connected.lock().map(|g| *g).unwrap_or(false);

                if !tmux_ready {
                    if let Ok(mut buffer_guard) = state.stderr_buffer.lock() {
                        buffer_guard.extend_from_slice(data);
                    }
                }
            }
            Err(nix::errno::Errno::EINTR) => continue,
            Err(_) => break,
        }
    }

    Ok(all_output)
}

fn process_direnv_output(
    output: &[u8],
    paths: &DaemonPaths,
    parent_pid: Pid,
) -> Result<(), Box<dyn std::error::Error>> {
    let output_str = String::from_utf8_lossy(output);
    let mut env_lines = Vec::new();

    for line in output_str.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("export ")
            || (trimmed.contains('=') && !trimmed.starts_with("direnv:"))
        {
            env_lines.push(trimmed);
        }
    }

    if !env_lines.is_empty() {
        let env_content = env_lines.join("\n");
        fs::write(&paths.temp_file, env_content)?;
        fs::rename(&paths.temp_file, &paths.env_file)?;
        signal::kill(parent_pid, Signal::SIGUSR1)?;
    }

    Ok(())
}

fn run_daemon(
    direnv_path: &str,
    envrc_dir: &Path,
    fifo_path: &Path,
    parent_pid: u32,
) -> Result<(), Box<dyn std::error::Error>> {
    let parent_pid = Pid::from_raw(parent_pid as i32);
    let daemon_pid = std::process::id();
    let daemon_start = Instant::now();

    // Set up paths and files
    let paths = setup_daemon_paths(envrc_dir, parent_pid, daemon_pid)?;
    let mut stderr_file_handle = File::create(&paths.stderr_file)?;

    // Set up Unix socket
    let _ = fs::remove_file(&paths.socket_path);
    let listener = UnixListener::bind(&paths.socket_path)?;

    // Create daemon state
    let state = DaemonState {
        stderr_buffer: Arc::new(Mutex::new(Vec::new())),
        pty_fd: Arc::new(Mutex::new(None)),
        tmux_connected: Arc::new(Mutex::new(false)),
    };

    // Start background threads
    start_lifetime_monitor(fifo_path);
    start_socket_listener(listener, &state, paths.temp_file.clone());
    start_tmux_launcher(daemon_pid, paths.temp_file.clone(), daemon_start);

    // Create PTY for direnv - get terminal attributes from current stderr if available
    let term_attrs = if let Ok(fd) = nix::fcntl::open("/dev/tty", OFlag::O_RDWR, Mode::empty()) {
        let attrs = termios::tcgetattr(&fd).ok();
        // fd will be closed automatically when dropped
        attrs
    } else {
        None
    };

    let OpenptyResult { master, slave } = openpty(None, term_attrs.as_ref())?;

    // Store PTY fd for tmux - we need to clone it for the state
    if let Ok(mut guard) = state.pty_fd.lock() {
        // Try to duplicate the master fd using nix
        if let Ok(dup_fd) = unistd::dup(&master) {
            *guard = Some(dup_fd);
        }
    }

    // Fork process to run direnv
    match unsafe { unistd::fork() }? {
        unistd::ForkResult::Parent { child } => {
            drop(slave);

            let all_output =
                handle_direnv_output(master.as_fd(), &mut stderr_file_handle, &state)?;

            // Wait for child to complete
            match waitpid(child, None)? {
                WaitStatus::Exited(_, 0) => {
                    process_direnv_output(&all_output, &paths, parent_pid)?;
                }
                _ => {}
            }

            // Cleanup
            let _ = fs::remove_file(&paths.temp_file);
            let _ = fs::remove_file(&paths.socket_path);

            // Keep daemon alive
            loop {
                thread::sleep(Duration::from_secs(60));
            }
        }
        unistd::ForkResult::Child => {
            drop(master);

            // dup2 with raw fd to avoid ownership issues
            let slave_raw = slave.as_raw_fd();
            unsafe {
                libc::dup2(slave_raw, 0);
                libc::dup2(slave_raw, 1);
                libc::dup2(slave_raw, 2);
            }

            if slave_raw > 2 {
                drop(slave);
            }

            let err = Command::new(direnv_path)
                .args(&["export", "zsh"])
                .current_dir(envrc_dir)
                .exec();

            eprintln!("Failed to exec direnv: {}", err);
            std::process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 && args[1] == "tmux" {
        // tmux subcommand
        if args.len() < 3 {
            eprintln!("Usage: direnv-instant tmux <daemon-pid>");
            std::process::exit(1);
        }

        if let Ok(daemon_pid) = args[2].parse::<u32>() {
            if let Err(e) = run_tmux_client(daemon_pid) {
                eprintln!("tmux client error: {}", e);
                std::process::exit(1);
            }
        } else {
            eprintln!("Invalid daemon PID");
            std::process::exit(1);
        }
    } else {
        // Default: run hook
        if let Err(e) = run_hook() {
            eprintln!("direnv-instant error: {}", e);
            std::process::exit(1);
        }
    }
}
