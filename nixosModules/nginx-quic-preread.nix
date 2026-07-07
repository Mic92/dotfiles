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
      the ngx_stream_quic_preread module. It extracts the SNI and ALPN from the
      QUIC Initial packet into $quic_preread_server_name and
      $quic_preread_alpn_protocols so QUIC/UDP can be routed by server name in a
      stream {} block without terminating TLS (the QUIC analogue of ssl_preread).
      Enable it per-server with `quic_preread on;`
    '';

    module = lib.mkOption {
      type = lib.types.attrs;
      default = pkgs.callPackage ../pkgs/nginx-quic-preread/package.nix { };
      defaultText = lib.literalExpression "pkgs.callPackage ../pkgs/nginx-quic-preread/package.nix { }";
      description = ''
        The nginx module descriptor, compiled statically into nginx via
        `services.nginx.additionalModules`. nginx must be built with stream
        support (the default `--with-stream`).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.nginx.additionalModules = [ cfg.module ];
  };
}
