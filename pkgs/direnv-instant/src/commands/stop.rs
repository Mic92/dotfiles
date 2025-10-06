use crate::daemon::{get_socket_path, stop_daemon};
use std::env;
use std::path::PathBuf;

pub fn run() {
    if let Ok(dir) = env::var("__DIRENV_INSTANT_CURRENT_DIR") {
        let socket_path = get_socket_path(&PathBuf::from(dir));
        stop_daemon(&socket_path);
    }
}
