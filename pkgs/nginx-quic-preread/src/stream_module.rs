//! nginx `stream {}` module exposing QUIC preread variables.
//!
//! This is the QUIC/UDP counterpart to nginx's built-in
//! `ngx_stream_ssl_preread_module` (which only works for TLS-over-TCP). It
//! registers a handler in the stream **preread** phase that inspects the first
//! datagram of a connection; if it is a QUIC Initial packet, it decrypts it and
//! extracts the ClientHello SNI + ALPN (see [`crate::quic`]) and publishes them
//! as run-time variables:
//!
//! * `$quic_preread_server_name`      — the SNI host name
//! * `$quic_preread_alpn_protocols`   — comma-separated ALPN list
//!
//! The directive `quic_preread on;` enables it inside a `server {}` block, e.g.
//!
//! ```nginx
//! stream {
//!     map $quic_preread_server_name $upstream {
//!         hostnames;
//!         a.example.org  10.0.0.1:8443;
//!         default        10.0.0.2:8443;
//!     }
//!     server {
//!         listen 443 udp reuseport;
//!         quic_preread on;
//!         proxy_pass $upstream;
//!     }
//! }
//! ```
//!
//! The layer is thin, unsafe FFI glue modeled directly on the reference C
//! module; all of the interesting logic lives in the fully-tested
//! [`crate::quic`] module. It is only compiled with the `nginx` feature, which
//! pulls in `ngx`/`nginx-sys` and therefore requires an NGINX source tree at
//! build time (see `package.nix`).

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use core::ffi::{c_char, c_uint, c_void};
use core::mem::{offset_of, size_of};
use core::ptr;

use ngx::ffi::*;

use crate::quic::quic_preread;

// nginx return codes we use (from ngx_core.h).
const NGX_OK_: ngx_int_t = NGX_OK as ngx_int_t;
const NGX_ERROR_: ngx_int_t = NGX_ERROR as ngx_int_t;
const NGX_AGAIN_: ngx_int_t = NGX_AGAIN as ngx_int_t;
const NGX_DECLINED_: ngx_int_t = NGX_DECLINED as ngx_int_t;

/// `#define NGX_CONF_UNSET (ngx_uint_t) -1` — bindgen can't reliably emit the
/// cast macro, so define the flag sentinel ourselves.
const NGX_CONF_UNSET_FLAG: ngx_flag_t = -1;

// ---------------------------------------------------------------------------
// Per-server configuration: `quic_preread on|off;`
// ---------------------------------------------------------------------------

#[repr(C)]
struct SrvConf {
    enabled: ngx_flag_t,
}

// ---------------------------------------------------------------------------
// Per-connection state: the extracted SNI / ALPN, allocated from the
// connection pool so the strings outlive the preread phase.
// ---------------------------------------------------------------------------

#[repr(C)]
struct PrereadCtx {
    server_name: ngx_str_t,
    alpn: ngx_str_t,
}

// ---------------------------------------------------------------------------
// Small FFI helpers replicating nginx's C macros.
// ---------------------------------------------------------------------------

/// `ngx_stream_conf_ctx_t->main_conf[idx]`.
unsafe fn stream_main_conf(cf: *mut ngx_conf_t, idx: ngx_uint_t) -> *mut c_void {
    let ctx = (*cf).ctx as *mut ngx_stream_conf_ctx_t;
    *(*ctx).main_conf.add(idx)
}

/// `s->srv_conf[quic_preread_module.ctx_index]`.
unsafe fn get_srv_conf(s: *mut ngx_stream_session_t) -> *mut SrvConf {
    let idx = ngx_stream_quic_preread_module.ctx_index;
    *(*s).srv_conf.add(idx) as *mut SrvConf
}

/// `s->ctx[quic_preread_module.ctx_index]`.
unsafe fn get_module_ctx(s: *mut ngx_stream_session_t) -> *mut PrereadCtx {
    let idx = ngx_stream_quic_preread_module.ctx_index;
    *(*s).ctx.add(idx) as *mut PrereadCtx
}

unsafe fn set_module_ctx(s: *mut ngx_stream_session_t, c: *mut PrereadCtx) {
    let idx = ngx_stream_quic_preread_module.ctx_index;
    *(*s).ctx.add(idx) = c as *mut c_void;
}

/// Copy `bytes` into `pool` and return an `ngx_str_t` pointing at the copy.
unsafe fn pool_str(pool: *mut ngx_pool_t, bytes: &[u8]) -> ngx_str_t {
    if bytes.is_empty() {
        return ngx_str_t::empty();
    }
    let p = ngx_pnalloc(pool, bytes.len()) as *mut u_char;
    if p.is_null() {
        return ngx_str_t::empty();
    }
    ptr::copy_nonoverlapping(bytes.as_ptr(), p, bytes.len());
    ngx_str_t {
        len: bytes.len(),
        data: p,
    }
}

// ---------------------------------------------------------------------------
// Preread-phase handler.
// ---------------------------------------------------------------------------

/// Runs in the stream preread phase. For UDP listeners nginx has already placed
/// the first datagram in `c->buffer`, so on the QUIC path this fires once with
/// the complete Initial packet.
unsafe extern "C" fn quic_preread_handler(s: *mut ngx_stream_session_t) -> ngx_int_t {
    let sscf = get_srv_conf(s);
    if sscf.is_null() || (*sscf).enabled != 1 {
        return NGX_DECLINED_;
    }

    // Already handled this connection.
    if !get_module_ctx(s).is_null() {
        return NGX_OK_;
    }

    let c = (*s).connection;
    if c.is_null() {
        return NGX_ERROR_;
    }

    let buf = (*c).buffer;
    if buf.is_null() {
        // TCP would keep buffering; for UDP the datagram is expected to be here
        // already. Ask for more and let the phase engine decide/timeout.
        return NGX_AGAIN_;
    }
    let pos = (*buf).pos;
    let last = (*buf).last;
    if pos.is_null() || last.is_null() || last <= pos {
        return NGX_AGAIN_;
    }
    let len = last.offset_from(pos) as usize;
    let data = core::slice::from_raw_parts(pos, len);

    // Allocate (zeroed) per-connection state up front and mark the connection
    // handled — even a non-QUIC / undecryptable datagram should fall through to
    // default routing rather than being retried.
    let ctx = ngx_pcalloc((*c).pool, size_of::<PrereadCtx>()) as *mut PrereadCtx;
    if ctx.is_null() {
        return NGX_ERROR_;
    }
    set_module_ctx(s, ctx);

    if let Ok(info) = quic_preread(data) {
        if let Some(name) = info.server_name {
            (*ctx).server_name = pool_str((*c).pool, name.as_bytes());
        }
        if !info.alpn.is_empty() {
            let joined = info.alpn.join(",");
            (*ctx).alpn = pool_str((*c).pool, joined.as_bytes());
        }
    }

    NGX_OK_
}

// ---------------------------------------------------------------------------
// Variable getters.
// ---------------------------------------------------------------------------

unsafe fn set_value(v: *mut ngx_stream_variable_value_t, s: &ngx_str_t) {
    if s.len == 0 || s.data.is_null() {
        (*v).set_valid(0);
        (*v).set_not_found(1);
        return;
    }
    (*v).set_len(s.len as c_uint);
    (*v).set_valid(1);
    (*v).set_no_cacheable(0);
    (*v).set_not_found(0);
    (*v).set_escape(0);
    (*v).data = s.data;
}

unsafe extern "C" fn variable_server_name(
    s: *mut ngx_stream_session_t,
    v: *mut ngx_stream_variable_value_t,
    _data: usize,
) -> ngx_int_t {
    let ctx = get_module_ctx(s);
    if ctx.is_null() {
        (*v).set_valid(0);
        (*v).set_not_found(1);
    } else {
        set_value(v, &(*ctx).server_name);
    }
    NGX_OK_
}

unsafe extern "C" fn variable_alpn(
    s: *mut ngx_stream_session_t,
    v: *mut ngx_stream_variable_value_t,
    _data: usize,
) -> ngx_int_t {
    let ctx = get_module_ctx(s);
    if ctx.is_null() {
        (*v).set_valid(0);
        (*v).set_not_found(1);
    } else {
        set_value(v, &(*ctx).alpn);
    }
    NGX_OK_
}

// ---------------------------------------------------------------------------
// Configuration lifecycle.
// ---------------------------------------------------------------------------

unsafe extern "C" fn preconfiguration(cf: *mut ngx_conf_t) -> ngx_int_t {
    // `ngx_string!` yields an `ngx_str_t`; add_variable copies the name into the
    // configuration pool, so passing a pointer to the local is fine.
    let mut name = ngx::ngx_string!("quic_preread_server_name");
    let var = ngx_stream_add_variable(cf, &mut name, 0);
    if var.is_null() {
        return NGX_ERROR_;
    }
    (*var).get_handler = Some(variable_server_name);
    (*var).data = 0;

    let mut name = ngx::ngx_string!("quic_preread_alpn_protocols");
    let var = ngx_stream_add_variable(cf, &mut name, 0);
    if var.is_null() {
        return NGX_ERROR_;
    }
    (*var).get_handler = Some(variable_alpn);
    (*var).data = 0;

    NGX_OK_
}

unsafe extern "C" fn postconfiguration(cf: *mut ngx_conf_t) -> ngx_int_t {
    let cmcf =
        stream_main_conf(cf, ngx_stream_core_module.ctx_index) as *mut ngx_stream_core_main_conf_t;
    if cmcf.is_null() {
        return NGX_ERROR_;
    }
    let phase = &mut (*cmcf).phases[ngx_stream_phases_NGX_STREAM_PREREAD_PHASE as usize];
    let h = ngx_array_push(&mut phase.handlers) as *mut ngx_stream_handler_pt;
    if h.is_null() {
        return NGX_ERROR_;
    }
    *h = Some(quic_preread_handler);
    NGX_OK_
}

unsafe extern "C" fn create_srv_conf(cf: *mut ngx_conf_t) -> *mut c_void {
    let conf = ngx_pcalloc((*cf).pool, size_of::<SrvConf>()) as *mut SrvConf;
    if conf.is_null() {
        return ptr::null_mut();
    }
    (*conf).enabled = NGX_CONF_UNSET_FLAG;
    conf as *mut c_void
}

unsafe extern "C" fn merge_srv_conf(
    _cf: *mut ngx_conf_t,
    prev: *mut c_void,
    conf: *mut c_void,
) -> *mut c_char {
    let prev = &*(prev as *mut SrvConf);
    let conf = &mut *(conf as *mut SrvConf);
    // ngx_conf_merge_value(conf->enabled, prev->enabled, 0)
    if conf.enabled == NGX_CONF_UNSET_FLAG {
        conf.enabled = if prev.enabled == NGX_CONF_UNSET_FLAG {
            0
        } else {
            prev.enabled
        };
    }
    ptr::null_mut()
}

// ---------------------------------------------------------------------------
// Module wiring.
// ---------------------------------------------------------------------------

/// `ngx_stream_module_t` is not `Sync`, but nginx only reads it. Wrap so we can
/// hold it in a plain `static`.
struct ModuleCtx(ngx_stream_module_t);
unsafe impl Sync for ModuleCtx {}

static NGX_STREAM_QUIC_PREREAD_MODULE_CTX: ModuleCtx = ModuleCtx(ngx_stream_module_t {
    preconfiguration: Some(preconfiguration),
    postconfiguration: Some(postconfiguration),
    create_main_conf: None,
    init_main_conf: None,
    create_srv_conf: Some(create_srv_conf),
    merge_srv_conf: Some(merge_srv_conf),
});

struct Commands([ngx_command_t; 2]);
unsafe impl Sync for Commands {}

static NGX_STREAM_QUIC_PREREAD_COMMANDS: Commands = Commands([
    ngx_command_t {
        name: ngx::ngx_string!("quic_preread"),
        type_: (NGX_STREAM_MAIN_CONF as ngx_uint_t)
            | (NGX_STREAM_SRV_CONF as ngx_uint_t)
            | (NGX_CONF_FLAG as ngx_uint_t),
        set: Some(ngx_conf_set_flag_slot),
        conf: NGX_STREAM_SRV_CONF_OFFSET as ngx_uint_t,
        offset: offset_of!(SrvConf, enabled) as ngx_uint_t,
        post: ptr::null_mut(),
    },
    ngx_command_t::empty(),
]);

/// The module definition. Referenced `extern` from the `ngx_modules.c` that the
/// nginx buildsystem generates for a statically linked (`--add-module`) module,
/// so we export only this symbol — no `ngx_modules!` array.
#[no_mangle]
pub static mut ngx_stream_quic_preread_module: ngx_module_t = ngx_module_t {
    ctx: &NGX_STREAM_QUIC_PREREAD_MODULE_CTX.0 as *const ngx_stream_module_t as *mut c_void,
    commands: &NGX_STREAM_QUIC_PREREAD_COMMANDS.0[0] as *const ngx_command_t as *mut ngx_command_t,
    type_: NGX_STREAM_MODULE as ngx_uint_t,
    ..ngx_module_t::default()
};
