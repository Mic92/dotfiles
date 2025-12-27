{
  self,
  ...
}:
{
  imports = [ self.nixosModules.authelia ];

  services.authelia.instances.main = {
    enable = true;

    settings = {
      authentication_backend = {
        password_change.disable = true;
        password_reset.disable = true;
        ldap.address = "ldap://localhost:389";
      };

      session.cookies = [
        {
          domain = "thalheim.io";
          authelia_url = "https://auth-eva.thalheim.io";
        }
      ];

      # No SMTP needed since password reset is disabled
      notifier.filesystem.filename = "/var/lib/authelia-main/notifications.txt";

      access_control.rules = [
        {
          domain = [
            "prometheus.thalheim.io"
            "alertmanager.thalheim.io"
          ];
          policy = "one_factor";
          subject = [ "group:prometheus" ];
        }
      ];
    };
  };

  services.nginx.virtualHosts."auth-eva.thalheim.io" = {
    enableACME = true;
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
