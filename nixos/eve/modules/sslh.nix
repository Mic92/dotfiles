{ lib, ... }:
with lib;

{
  config = {
    services.sslh = {
      enable = true;
      transparent = true;
      port = 443;
      appendConfig = ''
        protocols:
        (
          { name: "ssh"; service: "ssh"; host: "localhost"; port: "22"; probe: "builtin"; },
          { name: "xmpp"; host: "localhost"; port: "5222"; probe: "builtin"; },
          { name: "ssl"; host: "localhost"; port: "4443"; probe: "builtin"; },
          { name: "tinc"; host: "localhost"; port: "655"; probe: "builtin"; }
        );
      '';
    };
  };
}
