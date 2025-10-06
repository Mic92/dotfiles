use crate::daemon::stop_daemon;
use nix::sys::select::{FdSet, select};
use nix::sys::signal::{SaFlags, SigAction, SigHandler, SigSet, Signal, sigaction};
use nix::sys::time::{TimeVal, TimeValLike};
use nix::unistd::read;
use std::fs::File;
use std::io::{self, Write};
use std::os::fd::AsFd;
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

static WATCH_RUNNING: AtomicBool = AtomicBool::new(true);

extern "C" fn sigint_handler(_: nix::libc::c_int) {
    WATCH_RUNNING.store(false, Ordering::SeqCst);
}

pub fn run(log_path: &Path, socket_path: &Path) {
    let handler = SigHandler::Handler(sigint_handler);
    let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());
    unsafe {
        let _ = sigaction(Signal::SIGINT, &action);
    }

    // Open log file for reading (should exist by now, but wait up to 5 seconds as safety margin)
    let log_file = {
        let start = Instant::now();
        let timeout = Duration::from_secs(5);
        loop {
            if let Ok(f) = File::open(log_path) {
                break f;
            }
            if start.elapsed() > timeout {
                eprintln!("Timeout waiting for log file");
                std::process::exit(1);
            }
            std::thread::sleep(Duration::from_millis(100));
        }
    };

    // Connect to daemon socket - will be closed when daemon exits
    let socket = UnixStream::connect(socket_path).unwrap_or_else(|_| std::process::exit(1));

    let mut buf = [0u8; 8192];
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    while WATCH_RUNNING.load(Ordering::SeqCst) {
        let mut fds = FdSet::new();
        fds.insert(log_file.as_fd());
        fds.insert(socket.as_fd());
        let mut timeout = TimeVal::milliseconds(100);

        match select(None, Some(&mut fds), None, None, Some(&mut timeout)) {
            Ok(_) => {
                // Check if log file has new data
                if fds.contains(log_file.as_fd()) {
                    match read(&log_file, &mut buf) {
                        Ok(0) => {
                            // No data available yet, continue
                        }
                        Ok(n) => {
                            let _ = handle.write_all(&buf[..n]);
                            let _ = handle.flush();
                        }
                        Err(_) => {}
                    }
                }

                // Check if socket closed (daemon done)
                if fds.contains(socket.as_fd()) {
                    match read(&socket, &mut buf) {
                        Ok(0) => {
                            // Socket closed - daemon is done, output remaining log data and exit
                            while let Ok(n) = read(&log_file, &mut buf) {
                                if n == 0 {
                                    break;
                                }
                                let _ = handle.write_all(&buf[..n]);
                            }
                            let _ = handle.flush();
                            break;
                        }
                        Ok(_) => {
                            // Unexpected data on socket, ignore
                        }
                        Err(_) => break,
                    }
                }
            }
            Err(_) => break,
        }
    }

    if !WATCH_RUNNING.load(Ordering::SeqCst) {
        stop_daemon(socket_path);
    }
}
