{ ... }:
{
  imports = [ ../../../nixosModules/flakelet-relay ];

  services.flakelet-relay = {
    acmeHost = "thalheim.io";
    domain = "flakelet.thalheim.io";
  };
  services.nginx.virtualHosts."flakelet.thalheim.io".useACMEHost = "thalheim.io";

  services.flakelet-agent.flakelets = [
    "tribuchet-hub"
    "nixbot"
  ];

  # One public client for both `flakelet-push login` (device flow) and the
  # dashboard on either relay (authorization code + PKCE); the relay checks
  # the id_token audience, so they must share the client_id.
  services.authelia.instances.main.settings.identity_providers.oidc = {
    claims_policies.flakelet-push.id_token = [
      "email"
      "groups"
    ];
    lifespans.custom.flakelet-push = {
      id_token = "1 hour";
      refresh_token = "1 month";
    };
    authorization_policies.flakelet = {
      default_policy = "deny";
      rules = [
        {
          policy = "one_factor";
          subject = [ "group:flakelet" ];
        }
      ];
    };
    clients = [
      {
        client_id = "flakelet-push";
        client_name = "flakelet";
        public = true;
        token_endpoint_auth_method = "none";
        require_pkce = true;
        pkce_challenge_method = "S256";
        grant_types = [
          "authorization_code"
          "urn:ietf:params:oauth:grant-type:device_code"
          "refresh_token"
        ];
        redirect_uris = [
          "https://flakelet.thalheim.io/ui/callback"
          "https://eva.thalheim.io/ui/callback"
        ];
        scopes = [
          "openid"
          "offline_access"
          "email"
          "profile"
          "groups"
        ];
        claims_policy = "flakelet-push";
        lifespan = "flakelet-push";
        authorization_policy = "flakelet";
      }
    ];
  };
  services.lldap.ensureGroups = [ "flakelet" ];
}
