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
        config.clan.core.vars.generators.authelia.files.ldap-password.path;
    };

    settings = {
      authentication_backend = {
        password_change.disable = false;
        password_reset.disable = false;
        ldap.address = "ldap://eve.r:389";
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

      identity_providers.oidc.clients = [
        {
          client_id = "buildbot";
          client_name = "Buildbot";
          client_secret = config.clan.core.vars.generators.buildbot-oidc.files.client-secret-hash.value;
          redirect_uris = [ "https://buildbot.thalheim.io/auth/oidc/callback" ];
          scopes = [
            "openid"
            "email"
            "profile"
            "groups"
          ];
          authorization_policy = "one_factor";
          # buildbot-nix sends credentials in the POST body
          token_endpoint_auth_method = "client_secret_post";
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
          domain = "flood.thalheim.io";
          policy = "one_factor";
          subject = [ "group:flood" ];
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
