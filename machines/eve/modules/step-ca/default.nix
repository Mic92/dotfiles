# Self-hosted ACME CA using step-ca with ca.r domain
{ config, pkgs, ... }:
let
  domain = "ca.r";
in
{
  security.acme = {
    acceptTerms = true;
    certs.${domain}.server = "https://${domain}:1443/acme/acme/directory";
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts.${domain} = {
      addSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "https://localhost:1443";
      };
      locations."= /ca.crt".alias =
        config.clan.core.vars.generators.step-intermediate-cert.files."intermediate.crt".path;
    };
  };

  # Clan vars generators for certificate generation
  clan.core.vars.generators = {
    # Root CA generator
    "step-ca" = {
      files."ca.key" = {
        secret = true;
        deploy = false;
      };
      files."ca.crt".secret = false;
      runtimeInputs = [ pkgs.step-cli ];
      script = ''
        step certificate create --template ${pkgs.writeText "root.tmpl" ''
          {
            "subject": {{ toJson .Subject }},
            "issuer": {{ toJson .Subject }},
            "keyUsage": ["certSign", "crlSign"],
            "basicConstraints": {
              "isCA": true,
              "maxPathLen": 1
            }
          }
        ''} "Krebs Root CA" $out/ca.crt $out/ca.key \
          --kty EC --curve P-256 \
          --no-password --insecure
      '';
    };

    # Intermediate key generator
    "step-intermediate-key" = {
      files."intermediate.key" = {
        secret = true;
        deploy = true;
        owner = "step-ca";
        group = "step-ca";
      };
      runtimeInputs = [ pkgs.step-cli ];
      script = ''
        step crypto keypair --kty EC --curve P-256 --no-password --insecure $out/intermediate.pub $out/intermediate.key
      '';
    };

    # Intermediate certificate generator
    "step-intermediate-cert" = {
      files."intermediate.crt".secret = false;
      dependencies = [
        "step-ca"
        "step-intermediate-key"
      ];
      runtimeInputs = [ pkgs.step-cli ];
      script = ''
        # Create intermediate certificate
        step certificate create \
          --ca $in/step-ca/ca.crt \
          --ca-key $in/step-ca/ca.key \
          --ca-password-file /dev/null \
          --key $in/step-intermediate-key/intermediate.key \
          --template ${pkgs.writeText "intermediate.tmpl" ''
            {
              "subject": {{ toJson .Subject }},
              "keyUsage": ["certSign", "crlSign"],
              "basicConstraints": {
                "isCA": true,
                "maxPathLen": 0
              },
              "nameConstraints": {
                "critical": true,
                "permittedDNSDomains": ["r", "w"]
              }
            }
          ''} \
          --not-after 8760h \
          --no-password --insecure \
          "Krebs Intermediate CA" \
          $out/intermediate.crt
      '';
    };
  };

  services.step-ca = {
    enable = true;
    intermediatePasswordFile = "/dev/null";
    address = "0.0.0.0";
    port = 1443;
    settings = {
      root = config.clan.core.vars.generators.step-ca.files."ca.crt".path;
      crt = config.clan.core.vars.generators.step-intermediate-cert.files."intermediate.crt".path;
      key = config.clan.core.vars.generators.step-intermediate-key.files."intermediate.key".path;
      dnsNames = [ domain ];
      logger.format = "text";
      db = {
        type = "badger";
        dataSource = "/var/lib/step-ca/db";
      };
      authority = {
        provisioners = [
          {
            type = "ACME";
            name = "acme";
            forceCN = true;
          }
        ];
        claims = {
          maxTLSCertDuration = "2160h";
          defaultTLSCertDuration = "2160h";
        };
        backdate = "1m0s";
      };
      tls = {
        cipherSuites = [
          "TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256"
          "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256"
        ];
        minVersion = 1.2;
        maxVersion = 1.3;
        renegotiation = false;
      };
    };
  };
}
