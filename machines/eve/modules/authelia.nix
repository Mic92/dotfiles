{
  config,
  self,
  ...
}:
{
  imports = [ self.nixosModules.authelia ];

  services.authelia.instances.main = {
    enable = true;

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
