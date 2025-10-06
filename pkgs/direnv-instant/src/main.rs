mod commands;
mod daemon;

use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1).map(|s| s.as_str()) {
        Some("start") => commands::start::run(),
        Some("stop") => commands::stop::run(),
        Some("watch") => {
            if args.len() < 4 {
                eprintln!("Usage: {} watch <fifo_path> <socket_path>", args[0]);
                std::process::exit(1);
            }
            commands::watch::run(Path::new(&args[2]), Path::new(&args[3]));
        }
        Some("hook") => {
            if args.len() < 3 {
                eprintln!("Usage: {} hook <zsh|bash>", args[0]);
                std::process::exit(1);
            }
            commands::hook::run(&args[2]);
        }
        _ => {
            eprintln!("Usage: {} <start|stop|watch|hook>", args[0]);
            std::process::exit(1);
        }
    }
}
