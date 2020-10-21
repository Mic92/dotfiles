{ lib, ... }:
with lib;

{
  services.sslh = {
    enable = true;
    transparent = true;
    port = 443;
    listenAddresses = [ "0.0.0.0" ];
    appendConfig = ''
      protocols:
      (
        { name: "ssh"; service: "ssh"; host: "localhost"; port: "22"; probe: "builtin"; },
        { name: "xmpp"; host: "localhost"; port: "5222"; probe: "builtin"; },
        { name: "tls"; host: "localhost"; port: "4443"; probe: "builtin"; },
        { name: "tinc"; host: "localhost"; port: "655"; probe: "builtin"; }
      );
    '';
  };

  # leads to port binding conflicts with nginx sometimes
  systemd.services.sslh.restartIfChanged = false;
}
