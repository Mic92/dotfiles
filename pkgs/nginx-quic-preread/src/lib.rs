//! QUIC preread for nginx `stream {}` — extract the TLS ClientHello SNI/ALPN
//! from a QUIC Initial packet so UDP/QUIC traffic can be routed by server name
//! without terminating TLS. The QUIC analogue of `ssl_preread` for TCP.
//!
//! * [`quic`] — pure-Rust QUIC Initial decryption + ClientHello parsing (unit
//!   tested against the RFC 9001 / RFC 9369 vectors, no nginx dependency).
//! * `stream_module` — the nginx dynamic stream module glue (compiled with the
//!   default `nginx` feature; requires an nginx source tree to build).

pub mod quic;

pub use quic::{quic_preread, PrereadError, PrereadInfo};

#[cfg(feature = "nginx")]
mod stream_module;
