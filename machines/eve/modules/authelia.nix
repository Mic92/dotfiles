{
  config,
  pkgs,
  self,
  ...
}:
{
  imports = [ self.nixosModules.authelia ];

  # Separate generator from the shared `authelia` one so adding OIDC
  # does not rotate its existing secrets.
  clan.core.vars.generators.authelia-oidc = {
    files.hmac-secret.owner = "authelia-main";
    files.issuer-key.owner = "authelia-main";
    runtimeInputs = with pkgs; [
      coreutils
      openssl
    ];
    script = ''
      openssl rand 64 | openssl base64 -A | tr '+/' '-_' | tr -d '=' > "$out/hmac-secret"
      openssl genrsa 4096 > "$out/issuer-key"
    '';
  };

  services.authelia.instances.main = {
    enable = true;

    secrets = {
      oidcHmacSecretFile = config.clan.core.vars.generators.authelia-oidc.files.hmac-secret.path;
      oidcIssuerPrivateKeyFile = config.clan.core.vars.generators.authelia-oidc.files.issuer-key.path;
    };

    environmentVariables = {
      AUTHELIA_NOTIFIER_SMTP_PASSWORD_FILE =
        config.clan.core.vars.generators.lldap-authelia.files.bind-password.path;
    };

    settings = {
      authentication_backend = {
        password_change.disable = false;
        password_reset.disable = false;
      };

      session.cookies = [
        {
          domain = "thalheim.io";
          authelia_url = "https://auth.thalheim.io";
        }
        {
          domain = "devkid.net";
          authelia_url = "https://auth.devkid.net";
        }
      ];

      notifier.smtp = {
        address = "smtp://mail.thalheim.io:587";
        username = "authelia@thalheim.io";
        sender = "authelia@thalheim.io";
      };

      # Only Jörg may authorize the herdr-eternal client.
      identity_providers.oidc.authorization_policies.herdr-eternal = {
        default_policy = "deny";
        rules = [
          {
            policy = "one_factor";
            subject = [ "user:joerg@thalheim.io" ];
          }
        ];
      };

      # Authelia's default refresh-token lifetime (~90 minutes) logs
      # herdr-eternal-ssh out as soon as a laptop sleeps for a while; keep the
      # refresh token valid for months so long-running herdr sessions can keep
      # re-minting access tokens.
      identity_providers.oidc.lifespans.custom.herdr-eternal = {
        access_token = "1 hour";
        id_token = "1 hour";
        refresh_token = "3 months";
      };

      identity_providers.oidc.clients = [
        {
          # Public client for `step ca certificate --provisioner authelia`
          client_id = "step-ca";
          client_name = "step-ca";
          public = true;
          token_endpoint_auth_method = "none";
          redirect_uris = [
            "http://127.0.0.1"
            "http://localhost"
          ];
          scopes = [
            "openid"
            "email"
            "profile"
          ];
          authorization_policy = "one_factor";
        }
        {
          client_id = "buildbot";
          client_name = "Buildbot";
          client_secret = config.clan.core.vars.generators.buildbot-oidc.files.client-secret-hash.value;
          redirect_uris = [
            "https://nixbot.thalheim.io/auth/oidc/callback"
            # Legacy domain, kept until the buildbot.thalheim.io redirect is removed.
            "https://buildbot.thalheim.io/auth/oidc/callback"
          ];
          scopes = [
            "openid"
            "email"
            "profile"
            "groups"
          ];
          authorization_policy = "one_factor";
        }
        {
          # Device-code flow for herdr-eternal-ssh (herdr --remote transport).
          client_id = "herdr-eternal";
          client_name = "herdr-eternal";
          public = true;
          token_endpoint_auth_method = "none";
          grant_types = [
            "urn:ietf:params:oauth:grant-type:device_code"
            "refresh_token"
          ];
          scopes = [
            "openid"
            "offline_access"
          ];
          # Grant the client's own audience even though the device-code flow
          # never requests one, so access tokens carry aud = ["herdr-eternal"]
          # and the server can validate it strictly.
          audience = [ "herdr-eternal" ];
          requested_audience_mode = "implicit";
          # JWT-profile access tokens so herdr-eternal-server can validate
          # them offline against the JWKS.
          access_token_signed_response_alg = "RS256";
          authorization_policy = "herdr-eternal";
          lifespan = "herdr-eternal";
        }
        {
          client_id = "synapse";
          client_name = "Matrix (Synapse)";
          client_secret = config.clan.core.vars.generators.synapse-oidc.files.client-secret-hash.value;
          redirect_uris = [ "https://matrix.thalheim.io/_synapse/client/oidc/callback" ];
          scopes = [
            "openid"
            "email"
            "profile"
          ];
          authorization_policy = "one_factor";
        }
        {
          client_id = "punchcard";
          client_name = "Punchcard";
          client_secret = config.clan.core.vars.generators.punchcard-oidc.files.client-secret-hash.value;
          redirect_uris = [ "https://punchcard.thalheim.io/callback" ];
          scopes = [
            "openid"
            "email"
            "profile"
          ];
          authorization_policy = "one_factor";
        }
      ];

      access_control.rules = [
        {
          domain = [
            "rss.thalheim.io"
            "rss.devkid.net"
          ];
          policy = "one_factor";
          subject = [ "group:freshrss" ];
        }
        {
          domain = "n8n.thalheim.io";
          policy = "one_factor";
          subject = [ "group:n8n" ];
        }
        {
          domain = "paperless.thalheim.io";
          policy = "one_factor";
          subject = [ "group:paperless" ];
        }
        {
          domain = [
            "torrent.thalheim.io"
            "warez.thalheim.io"
            "warez-dav.thalheim.io"
          ];
          policy = "one_factor";
          subject = [ "group:torrent" ];
        }
        {
          domain = "pinchflat.thalheim.io";
          policy = "one_factor";
          subject = [ "group:pinchflat" ];
        }
      ];
    };
  };

  services.nginx.virtualHosts."auth.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:9091";
      extraConfig = ''
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
      '';
    };
  };

  services.nginx.virtualHosts."auth.devkid.net" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:9091";
      extraConfig = ''
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
      '';
    };
  };
}
