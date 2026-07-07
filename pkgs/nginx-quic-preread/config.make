ngx_addon_name=ngx_stream_quic_preread_module
ngx_cargo_manifest=$ngx_addon_dir/Cargo.toml

# generate the Makefile section that builds the Rust staticlib(s)
ngx_rust_make_modules
