{ ... }:
{
  imports = [ ../../../nixosModules/flakelet-relay ];

  services.flakelet-relay.acmeHost = "thalheim.io";

  services.flakelet-agent.flakelets = [
    "tribuchet-hub"
    "nixbot"
  ];

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
