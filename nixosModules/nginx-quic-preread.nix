{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.nginx.quicPreread;
in
{
  options.services.nginx.quicPreread = {
    enable = lib.mkEnableOption ''
      the ngx_stream_quic_preread module, which extracts the SNI and ALPN from
      the QUIC Initial packet into $quic_preread_server_name and
      $quic_preread_alpn_protocols so QUIC/UDP can be routed by server name in a
      stream {} block without terminating TLS (the QUIC analogue of ssl_preread)
    '';

    basePackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.nginxQuic;
      defaultText = lib.literalExpression "pkgs.nginxQuic";
      description = ''
        Base nginx package used both to run the server and to build the module
        against. It is rebuilt with `--with-compat` so the externally-built
        dynamic module can be loaded. Must include stream support (the default
        nginx build enables `--with-stream`).
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ../pkgs/nginx-quic-preread/package.nix {
        nginx = cfg.basePackage;
      };
      defaultText = lib.literalExpression "pkgs.callPackage ../pkgs/nginx-quic-preread/package.nix { nginx = cfg.basePackage; }";
      description = "The built ngx_stream_quic_preread dynamic module.";
    };
  };

  config = lib.mkIf cfg.enable {
    # A dynamic module can only be loaded by an nginx built with `--with-compat`;
    # rebuild the chosen base package with that flag. Both the module and this
    # nginx are built from the same source + `--with-compat`, so their module
    # signatures match.
    services.nginx.package = lib.mkForce (
      cfg.basePackage.overrideAttrs (old: {
        configureFlags = (old.configureFlags or [ ]) ++ [ "--with-compat" ];
      })
    );

    # `prependConfig` lands in the main context at the top of nginx.conf, which
    # is exactly where `load_module` must appear (before the stream {} block that
    # uses the module).
    services.nginx.prependConfig = ''
      load_module ${cfg.package}/lib/nginx/modules/ngx_stream_quic_preread_module.so;
    '';

    # QUIC preread only makes sense for a UDP stream listener.
    services.nginx.streamConfig = lib.mkDefault ''
      # Example: route QUIC by SNI without terminating TLS.
      #
      # map $quic_preread_server_name $quic_backend {
      #     hostnames;
      #     app.example.org  127.0.0.1:8443;
      #     default          127.0.0.1:9443;
      # }
      #
      # server {
      #     listen 443 udp reuseport;
      #     quic_preread on;
      #     proxy_pass $quic_backend;
      # }
    '';
  };
}
