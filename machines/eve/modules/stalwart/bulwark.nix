{ config, ... }:
{
  services.bulwark = {
    enable = true;
    hostname = "127.0.0.1";
    port = 8643;
    updateCheck.enabled = false;
    settings = {
      branding.appName = "thalheim.io mail";
      jmapServerUrl = "https://jmap.thalheim.io";
      sessionSecretFile = config.clan.core.vars.generators.bulwark.files.session-secret.path;
      settingsSyncEnabled = true;
    };
  };

  clan.core.vars.generators.bulwark = {
    files.session-secret.owner = "bulwark";
    script = ''
      head -c 32 /dev/urandom | base64 | tr -d '\n' > "$out/session-secret"
    '';
  };

  services.nginx.virtualHosts."mail.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:8643";
      proxyWebsockets = true;
      # next-intl turns X-Forwarded-Proto: https into an https:// self-rewrite
      recommendedProxySettings = false;
      extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Real-IP $remote_addr;
        client_max_body_size 256M;
      '';
    };
  };
}
