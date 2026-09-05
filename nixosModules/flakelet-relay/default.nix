# flakelet-relay on eve and eva: CI (nixbot id tokens) and I (Authelia
# device login or step-ca client cert) ask it to run `flakelet update`
# on agents that dial in with step-ca ACME certs over retiolum. Both
# relays are listed under _flakelet-relay._tcp.thalheim.io.
#
# The relay itself runs as a flakelet so CI can redeploy it through the
# agents; a restart drops streams but `push` fails over to the other relay.
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
  # Agent identity: ACME cert for <host>.r from step-ca (ca.r).
  agentHost = "${config.networking.hostName}.r";
in
{
  imports = [
    self.inputs.flakelet.nixosModules.flakelet
    self.inputs.flakelet-relay.nixosModules.agent
  ];

  options.services.flakelet-relay = {
    acmeHost = lib.mkOption {
      type = lib.types.str;
      description = "security.acme.certs entry served on :7443. Must cover <host>.thalheim.io, the SRV target.";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      description = "nginx vhost (TLS configured by the machine) for the dashboard and client API. Must be a redirect_uri of the flakelet-push Authelia client on eve.";
    };
  };

  config = {
    services.flakelets = {
      enable = true;
      services.flakelet-relay = {
        flake = "github:Mic92/flakelet-relay";
        autoUpdate.enable = true;
        settings = {
          certFile = "${cert.directory}/fullchain.pem";
          keyFile = "${cert.directory}/key.pem";
          clientCAFiles = [ "${clientCa}" ];
          settings = {
            name = config.networking.hostName;
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
                # Dashboard login; public client with PKCE.
                login.clientId = "flakelet-push";
              };
            };
            agents = {
              eve = [ "x509:dns:eve.r" ];
              eva = [ "x509:dns:eva.r" ];
              eliza = [ "x509:dns:eliza.r" ];
              jamie = [ "x509:dns:jamie.r" ];
            };
            groups.tum = [
              "eliza"
              "jamie"
            ];
            groups.relays = [
              "eve"
              "eva"
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
              flakelet-relay = {
                principals = [ "oidc:nixbot:repo:github:Mic92/flakelet-relay:ref:refs/heads/main" ];
                targets = [ "@relays/flakelet-relay" ];
              };
              admin = {
                principals = [
                  "x509:email:joerg@thalheim.io"
                  "oidc:authelia:groups:flakelet"
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
              principals = [ "oidc:nixbot:repo:github:Mic92/flakelet-relay:ref:refs/heads/main" ];
              targets = [
                "eve/flakelet-relay"
                "eva/flakelet-relay"
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
              principals = [ "oidc:authelia:groups:flakelet" ];
              targets = [ "jamie/anything" ];
            }
            {
              principals = [ "oidc:authelia:email:someone@thalheim.io" ];
              targets = [ "eve/nixbot" ];
              allow = false;
            }
          ];
        };
      };
    };

    security.acme.certs.${config.services.flakelet-relay.acmeHost}.reloadServices = [
      "flakelet-relay.service"
    ];

    # So the relay flakelet above can be pushed here.
    security.acme.certs.${agentHost} = {
      server = config.retiolum.ca.acmeURL;
      reloadServices = [ "flakelet-agent.service" ];
    };
    services.nginx.virtualHosts.${agentHost}.enableACME = true;
    systemd.services.flakelet-agent = rec {
      wants = [ "acme-${agentHost}.service" ];
      after = wants;
    };
    services.flakelet-agent = {
      enable = true;
      relaySrv = "thalheim.io";
      certFile = "/var/lib/acme/${agentHost}/fullchain.pem";
      keyFile = "/var/lib/acme/${agentHost}/key.pem";
      flakelets = [ "flakelet-relay" ];
    };

    services.nginx.virtualHosts.${config.services.flakelet-relay.domain} = {
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:7400";
        # SSE deploy/log streams.
        extraConfig = ''
          proxy_buffering off;
          proxy_read_timeout 3600s;
        '';
      };
    };

    networking.firewall.allowedTCPPorts = [ 7443 ];
  };
}
