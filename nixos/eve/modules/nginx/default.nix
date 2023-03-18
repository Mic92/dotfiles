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
    ./threema.nix
    ./retiolum.nix
    ./screenshare
    ./mta-sts.nix
    ./phd-website.nix
  ];

  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      config.listen = lib.mkDefault [
        {
          addr = "0.0.0.0";
          port = 443;
          ssl = true;
        }
        {
          addr = "[::1]";
          port = 443;
          ssl = true;
        }
        {
          addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
          port = 80;
        }
        {
          addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
          port = 443;
          ssl = true;
        }
        {
          addr = "[2a01:4f8:10b:49f::1]";
          port = 443;
          ssl = true;
        }
        {
          addr = "0.0.0.0";
          port = 80;
          ssl = false;
        }
        {
          addr = "[2a01:4f8:10b:49f::1]";
          port = 80;
          ssl = false;
        }
      ];
    });
  };

  config = {
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
