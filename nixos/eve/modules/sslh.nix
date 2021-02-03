{ lib, ... }:
with lib;

{
  services.sslh = {
    enable = true;
    port = 443;
    listenAddresses = [ "0.0.0.0" ];
    appendConfig = ''
      protocols:
      (
        { name: "ssh"; service: "ssh"; host: "localhost"; port: "22"; probe: "builtin"; log_level: 0; tfo_ok: true; keepalive: true; fork: true; },
        { name: "xmpp"; host: "localhost"; port: "5222"; probe: "builtin"; log_level: 0; tfo_ok: true },
        { name: "tls"; host: "localhost"; port: "5343"; sni_hostnames: [ "dns.thalheim.io" ]; log_level: 0; tfo_ok: true; },
        { name: "tls"; host: "localhost"; port: "4443"; probe: "builtin"; log_level: 0; tfo_ok: true; },
        { name: "tinc"; host: "localhost"; port: "655"; probe: "builtin"; log_level: 0; tfo_ok: true; }
      );
    '';
  };

  # leads to port binding conflicts with nginx sometimes
  systemd.services.sslh.restartIfChanged = false;
}
