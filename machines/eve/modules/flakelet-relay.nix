{ config, self, ... }:
{
  imports = [
    ../../../nixosModules/flakelet-relay
    self.inputs.flakelet-relay.nixosModules.agent
  ];

  services.flakelet-relay.acmeHost = "thalheim.io";

  # Agent identity: ACME cert for eve.r from step-ca (ca.r).
  security.acme.certs."eve.r" = {
    server = config.retiolum.ca.acmeURL;
    reloadServices = [ "flakelet-agent.service" ];
  };
  services.nginx.virtualHosts."eve.r".enableACME = true;

  services.flakelet-agent = {
    enable = true;
    relaySrv = "thalheim.io";
    certFile = "/var/lib/acme/eve.r/fullchain.pem";
    keyFile = "/var/lib/acme/eve.r/key.pem";
    flakelets = [
      "tribuchet-hub"
      "nixbot"
    ];
  };

  # `flakelet-push login --issuer https://auth.thalheim.io`
  services.authelia.instances.main.settings.identity_providers.oidc = {
    claims_policies.flakelet-push.id_token = [
      "email"
      "groups"
    ];
    lifespans.custom.flakelet-push = {
      id_token = "1 hour";
      refresh_token = "1 month";
    };
    clients = [
      {
        client_id = "flakelet-push";
        client_name = "flakelet-push";
        public = true;
        token_endpoint_auth_method = "none";
        grant_types = [
          "urn:ietf:params:oauth:grant-type:device_code"
          "refresh_token"
        ];
        scopes = [
          "openid"
          "offline_access"
          "email"
          "groups"
        ];
        claims_policy = "flakelet-push";
        lifespan = "flakelet-push";
        authorization_policy = "one_factor";
      }
    ];
  };
}
