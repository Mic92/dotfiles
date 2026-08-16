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

    # SSH user CA signing nixbot deploy certificates
    "step-ssh-user-ca" = {
      files."ca.key" = {
        secret = true;
        deploy = true;
        owner = "step-ca";
        group = "step-ca";
      };
      files."ca.pub".secret = false;
      runtimeInputs = [ pkgs.openssh ];
      script = ''
        ssh-keygen -t ed25519 -N "" -C "step-ca ssh user ca" -f $out/ca.key
        mv $out/ca.key.pub $out/ca.pub
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
          --template ${./intermediate.tmpl} \
          --not-after 8760h \
          --no-password --insecure \
          "Krebs Intermediate CA" \
          $out/intermediate.crt
      '';
    };
  };

  # Effects secrets for the intermediate-cert renewal effect (checks/effects.nix)
  clan.core.vars.generators.step-ca-renew-effect-secrets = {
    files.secrets.secret = true;
    dependencies = [
      "step-ca"
      "step-intermediate-key"
    ];
    runtimeInputs = [ pkgs.jq ];
    script = ''
      jq -n \
        --rawfile ca_crt $in/step-ca/ca.crt \
        --rawfile ca_key $in/step-ca/ca.key \
        --rawfile int_key $in/step-intermediate-key/intermediate.key \
        '{ "step-ca": { data: { "ca.crt": $ca_crt, "ca.key": $ca_key, "intermediate.key": $int_key }, condition: "isDefaultBranch" } }' \
        > $out/secrets
    '';
  };

  # step-ca fetches the Authelia OIDC discovery document once at startup and
  # permanently disables the provisioner if that fails, so wait until it is
  # actually served before starting.
  systemd.services.step-ca = {
    after = [
      "authelia-main.service"
      "nginx.service"
    ];
    preStart = ''
      ${pkgs.curl}/bin/curl -fsS --retry 60 --retry-all-errors --retry-delay 1 -o /dev/null \
        https://auth.thalheim.io/.well-known/openid-configuration \
        || echo "authelia OIDC discovery endpoint not ready" >&2
    '';
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
      ssh.userKey = config.clan.core.vars.generators.step-ssh-user-ca.files."ca.key".path;
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
          # SSH deploy certs for nixbot effects, principal = token sub
          {
            type = "OIDC";
            name = "nixbot";
            clientID = "step-ca-ssh";
            clientSecret = "";
            configurationEndpoint = "https://nixbot.thalheim.io/.well-known/openid-configuration";
            claims = {
              enableSSHCA = true;
              defaultUserSSHCertDuration = "10m";
              maxUserSSHCertDuration = "10m";
            };
            options.ssh.template = ''
              {
                "type": {{ toJson .Type }},
                "keyId": {{ toJson .KeyID }},
                "principals": [{{ toJson .Token.sub }}],
                "extensions": {{ toJson .Extensions }},
                "criticalOptions": {{ toJson .CriticalOptions }}
              }
            '';
          }
          # OAuth-backed client certs, e.g. mTLS for the gRPC nix-daemon
          {
            type = "OIDC";
            name = "authelia";
            clientID = "step-ca";
            clientSecret = "";
            configurationEndpoint = "https://auth.thalheim.io/.well-known/openid-configuration";
            claims = {
              maxTLSCertDuration = "168h";
              defaultTLSCertDuration = "24h";
            };
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
