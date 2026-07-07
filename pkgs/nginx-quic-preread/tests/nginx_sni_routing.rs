//! Integration test: drive a **real** nginx built with the module and verify it
//! routes QUIC by SNI.
//!
//! It spawns `$NGINX_BIN` (an nginx compiled with ngx_stream_quic_preread) with
//! a stream config that maps `$quic_preread_server_name` to one of two UDP
//! backends, fires a genuine QUIC Initial packet (built by the crate's own
//! encoder) at nginx, and asserts the datagram is proxied to the SNI-matched
//! backend and not the other.
//!
//! `$NGINX_BIN` must point at an nginx that has the module linked in; when it is
//! unset the test is skipped, so a plain `cargo test` without nginx still
//! succeeds. The Nix check `package-nginx-quic-preread-test-sni-routing` sets it.

use std::fs;
use std::io::ErrorKind;
use std::net::UdpSocket;
use std::path::PathBuf;
use std::process::{Child, Command};
use std::time::{Duration, Instant};

use ngx_quic_preread::quic::testvec::encode_initial;

const QUIC_V1: u32 = 1;

/// A spawned nginx that is killed on drop.
struct Nginx {
    child: Child,
    dir: PathBuf,
}

impl Drop for Nginx {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// Grab a free UDP port on loopback by binding and immediately dropping.
fn free_udp_port() -> u16 {
    let s = UdpSocket::bind("127.0.0.1:0").unwrap();
    s.local_addr().unwrap().port()
}

fn wait_for_log(path: &PathBuf, needle: &str, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        if let Ok(s) = fs::read_to_string(path) {
            if s.contains(needle) {
                return true;
            }
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    false
}

#[test]
fn routes_quic_by_sni() {
    let Some(nginx_bin) = std::env::var_os("NGINX_BIN") else {
        eprintln!("NGINX_BIN not set; skipping real-nginx integration test");
        return;
    };

    // Two backends nginx will proxy to, plus the port nginx listens on.
    let backend_routed = UdpSocket::bind("127.0.0.1:0").unwrap();
    let backend_default = UdpSocket::bind("127.0.0.1:0").unwrap();
    backend_routed
        .set_read_timeout(Some(Duration::from_millis(2000)))
        .unwrap();
    backend_default
        .set_read_timeout(Some(Duration::from_millis(2000)))
        .unwrap();
    let port_routed = backend_routed.local_addr().unwrap().port();
    let port_default = backend_default.local_addr().unwrap().port();
    let nginx_port = free_udp_port();

    // Prefix dir + config.
    let dir = std::env::temp_dir().join(format!("ngx-quic-preread-{}", std::process::id()));
    fs::create_dir_all(&dir).unwrap();
    let conf = dir.join("nginx.conf");
    let errlog = dir.join("error.log");
    let pid = dir.join("nginx.pid");

    fs::write(
        &conf,
        format!(
            r#"
worker_processes 1;
daemon off;
error_log {errlog} notice;
pid {pid};
events {{ worker_connections 64; }}
stream {{
    map $quic_preread_server_name $backend {{
        routed.example  127.0.0.1:{port_routed};
        default         127.0.0.1:{port_default};
    }}
    server {{
        listen 127.0.0.1:{nginx_port} udp;
        quic_preread on;
        proxy_pass $backend;
        proxy_responses 0;
        proxy_timeout 1s;
    }}
}}
"#,
            errlog = errlog.display(),
            pid = pid.display(),
        ),
    )
    .unwrap();

    let child = Command::new(&nginx_bin)
        .arg("-p")
        .arg(&dir)
        .arg("-e")
        .arg(&errlog)
        .arg("-c")
        .arg(&conf)
        .spawn()
        .expect("spawn nginx");
    let nginx = Nginx { child, dir };

    assert!(
        wait_for_log(&errlog, "start worker process", Duration::from_secs(5)),
        "nginx did not start; error.log:\n{}",
        fs::read_to_string(&errlog).unwrap_or_default()
    );

    let target = format!("127.0.0.1:{nginx_port}");

    // Case 1: SNI matches the map -> routed backend, not the default one.
    let sent = send_and_expect(&target, "routed.example", &backend_routed, &backend_default);
    assert!(sent, "matched backend did not receive the QUIC Initial");

    // Case 2: unknown SNI -> default backend, not the routed one.
    let sent = send_and_expect(
        &target,
        "unknown.example",
        &backend_default,
        &backend_routed,
    );
    assert!(sent, "default backend did not receive the QUIC Initial");

    drop(nginx);
}

/// Send a QUIC Initial with `sni` at `target`; assert `expect` receives a
/// non-empty QUIC long-header datagram and `other` receives nothing.
fn send_and_expect(target: &str, sni: &str, expect: &UdpSocket, other: &UdpSocket) -> bool {
    let datagram = encode_initial(QUIC_V1, sni, &["h3"]);
    let client = UdpSocket::bind("127.0.0.1:0").unwrap();
    client.send_to(&datagram, target).unwrap();

    let mut buf = [0u8; 2048];
    let got = match expect.recv_from(&mut buf) {
        Ok((n, _)) => n > 0 && (buf[0] & 0x80) != 0, // QUIC long header bit
        Err(e) if e.kind() == ErrorKind::WouldBlock || e.kind() == ErrorKind::TimedOut => false,
        Err(e) => panic!("recv on expected backend failed: {e}"),
    };

    // The other backend must not have been picked. Give it a short window.
    other
        .set_read_timeout(Some(Duration::from_millis(300)))
        .unwrap();
    match other.recv_from(&mut buf) {
        Ok((n, _)) => panic!("SNI {sni:?} was mis-routed: other backend got {n} bytes"),
        Err(e) if e.kind() == ErrorKind::WouldBlock || e.kind() == ErrorKind::TimedOut => {}
        Err(e) => panic!("recv on other backend failed: {e}"),
    }
    // restore the longer timeout for the next case
    other
        .set_read_timeout(Some(Duration::from_millis(2000)))
        .unwrap();

    got
}
