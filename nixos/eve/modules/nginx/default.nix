{ config
, lib
, ...
}: {
  imports = [
    ./devkid.net.nix
    ./dl.nix
    ./glowing-bear.nix
    ./homepage.nix
    ./ip.nix
    ./muc.nix
    ./retiolum.nix
    ./screenshare
    ./mergebot.nix
    ./mta-sts.nix
    ./phd-website.nix
    ./photoprism.nix
  ];

  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      config.quic = true;
      config.listen = lib.mkDefault [
        # localhost (dualstack)
        { addr = "[::1]"; port = 443; ssl = true; }
        # retiolum
        { addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]"; port = 80; }
        { addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]"; port = 443; ssl = true; }
        # ipv6 public
        { addr = "[${config.networking.eve.ipv6.address}]"; port = 80; ssl = false; }
        { addr = "[${config.networking.eve.ipv6.address}]"; port = 443; ssl = true; }
        # ipv4 public
        { addr = "0.0.0.0"; port = 80; ssl = false; }
        { addr = "0.0.0.0"; port = 443; ssl = true; }
      ];
    });
  };

  config = {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    networking.firewall.allowedUDPPorts = [ 443 ];

    services.nginx.commonHttpConfig = ''
      add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains; preload' always;
    '';
    services.logrotate.enable = true;
    # format:
    # RFC2136_NAMESERVER=ns1.thalheim.io
    # RFC2136_TSIG_ALGORITHM=hmac-sha256.
    # RFC2136_TSIG_KEY=acme
    # RFC2136_TSIG_SECRET="00000000000000000000000000000000000000000000"
    sops.secrets.lego-knot-credentials.owner = "acme";

    security.acme.certs =
      let
        sanCertificate = { rsa ? false }: {
          domain = "thalheim.io";
          postRun = "systemctl reload nginx.service";
          group = "nginx";
          keyType =
            if rsa
            then "rsa2048"
            else "ec384";
          dnsProvider = "rfc2136";
          extraDomainNames = [
            "*.thalheim.io"
            "devkid.net"
            "*.devkid.net"
            "lekwati.com"
            "*.lekwati.com"
          ];
          credentialsFile = config.sops.secrets.lego-knot-credentials.path;
        };
      in
      {
        "thalheim.io" = sanCertificate { };
        "legacy-thalheim.io" = sanCertificate { rsa = true; };
      };
  };
}
