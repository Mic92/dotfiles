# generate intermediate certificate with generate-krebs-intermediate-ca
{ config, lib, pkgs, ... }: let
  domain = "ca.r";
in {
  security.acme = {
    acceptTerms = true; # kinda pointless since we never use upstream
    certs.${domain}.server = "https://${domain}:1443/acme/acme/directory"; # use 1443 here cause bootstrapping loop
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts.${domain} = {
      addSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "https://localhost:1443";
      };
      # TODO
      locations."= /ca.crt".alias = config.clan.core.vars.generators.step-intermediate-cert.files."intermediate.crt".value;
    };
  };

  clan.core.vars.generators.step-ca = {
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
      ''} "Krebs Root CA" $out/ca.crt $out/ca.key
    '';
  };

  clan.core.vars.generators.step-intermediate-key = {
    files."intermediate.key" = {
      secret = true;
      deploy = true;
    };
    dependencies = [
      "step-intermediate-key"
    ];
    runtimeInputs = [
      pkgs.step-cli
    ];
    script = ''
      step crypto keypair --no-password --insecure $out/intermediate.pub $out/intermediate.key
    '';
  };

  clan.core.vars.generators.step-intermediate-cert = {
    files."intermediate.crt".secret = false;
    dependencies = [
      "step-ca"
      "step-intermediate-key"
    ];
    runtimeInputs = [
      pkgs.step-cli
    ];
    script = ''
      step certificate ca renew --ca $in/step-ca/ca.crt $in/step-ca/ca.key --offline --root $in/step-ca/ca.crt --ca-config ${pkgs.writeText "intermediate.tmpl" ''
        {
          "subject": {{ toJson .Subject }},
          "keyUsage": ["certSign", "crlSign"],
          "basicConstraints": {
            "isCA": true,
            "maxPathLen": 0
          },
          "nameConstraints": {
            "critical": true,
            "permittedDNSDomains": ["r" ,"w"]
          }
        }
      ''}
    '';
  };

  services.step-ca = {
    enable = true;
    intermediatePasswordFile = "/dev/null";
    address = "0.0.0.0";
    port = 1443;
    settings = {
      root = config.clan.core.vars.generators.step-intermediate-cert.files."ca.crt".value;
      #root = pkgs.writeText "root.crt" config.krebs.ssl.rootCA;
      crt = pkgs.writeText "intermediate.crt" config.krebs.ssl.intermediateCA;
      key = "/var/lib/step-ca/intermediate_ca.key";
      dnsNames = [ domain ];
      logger.format = "text";
      db = {
        type = "badger";
        dataSource = "/var/lib/step-ca/db";
      };
      authority = {
        provisioners = [{
          type = "ACME";
          name = "acme";
          forceCN = true;
        }];
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
