# flakelet-relay on eve and eva: CI (nixbot id tokens) and I (Authelia
# device login or step-ca client cert) ask it to run `flakelet update`
# on agents that dial in with step-ca ACME certs over retiolum. Both
# relays are listed under _flakelet-relay._tcp.thalheim.io.
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  # Agents present ACME certs from ca.r (kartei intermediate), my own
  # client cert comes from the Authelia OIDC provisioner (root).
  clientCa = pkgs.writeText "krebs-ca.pem" ''
    ${config.retiolum.ca.rootCA}
    ${config.retiolum.ca.intermediateCA}
  '';
  cert = config.security.acme.certs.${config.services.flakelet-relay.acmeHost};
in
{
  imports = [ self.inputs.flakelet-relay.nixosModules.relay ];

  options.services.flakelet-relay.acmeHost = lib.mkOption {
    type = lib.types.str;
    description = "security.acme.certs entry served on :7443. Must cover <host>.thalheim.io, the SRV target.";
  };

  config = {
    services.flakelet-relay = {
      enable = true;
      tls = {
        certFile = "${cert.directory}/fullchain.pem";
        keyFile = "${cert.directory}/key.pem";
        clientCAFiles = [ clientCa ];
      };
      settings = {
        listenHttp = "127.0.0.1:7400";
        listenTls = "[::]:7443";
        issuers = {
          nixbot = {
            url = "https://nixbot.thalheim.io";
            audience = "flakelet-relay";
          };
          # Authelia's sub is opaque, match on email and groups instead.
          authelia = {
            url = "https://auth.thalheim.io";
            audience = "flakelet-push";
            principalClaims = [
              "email"
              "groups"
            ];
          };
        };
        agents = {
          eve = [ "x509:dns:eve.r" ];
          eliza = [ "x509:dns:eliza.r" ];
          jamie = [ "x509:dns:jamie.r" ];
        };
        groups.tum = [
          "eliza"
          "jamie"
        ];
        rules = {
          tribuchet = {
            principals = [ "oidc:nixbot:repo:github:Mic92/tribuchet:ref:refs/heads/main" ];
            targets = [
              "eve/tribuchet-hub"
              "@tum/tribuchet-worker"
            ];
          };
          nixbot = {
            principals = [ "oidc:nixbot:repo:github:Mic92/nixbot:ref:refs/heads/main" ];
            targets = [ "eve/nixbot" ];
          };
          admin = {
            principals = [
              "x509:email:joerg@thalheim.io"
              "oidc:authelia:email:joerg@thalheim.io"
            ];
            targets = [ "*/*" ];
          };
        };
      };
      policyChecks = [
        {
          principals = [ "oidc:nixbot:repo:github:Mic92/tribuchet:ref:refs/heads/main" ];
          targets = [
            "eve/tribuchet-hub"
            "eliza/tribuchet-worker"
            "jamie/tribuchet-worker"
          ];
        }
        {
          principals = [ "oidc:nixbot:repo:github:Mic92/tribuchet:ref:refs/pull/1/merge" ];
          targets = [ "eve/tribuchet-hub" ];
          allow = false;
        }
        {
          principals = [ "oidc:nixbot:repo:github:Mic92/nixbot:ref:refs/heads/main" ];
          targets = [ "eve/tribuchet-hub" ];
          allow = false;
        }
        {
          principals = [ "oidc:authelia:email:joerg@thalheim.io" ];
          targets = [ "jamie/anything" ];
        }
      ];
    };

    security.acme.certs.${config.services.flakelet-relay.acmeHost}.reloadServices = [
      "flakelet-relay.service"
    ];

    networking.firewall.allowedTCPPorts = [ 7443 ];
  };
}
