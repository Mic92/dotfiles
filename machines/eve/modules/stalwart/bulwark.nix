{ config, ... }:
let
  primary = false;
  stalwartHost = if primary then "mail.thalheim.io" else "stalwart.thalheim.io";
  vhost = if primary then "webmail.thalheim.io" else "bulwark.thalheim.io";
in
{
  services.bulwark = {
    enable = true;
    hostname = "127.0.0.1";
    port = 8643;
    updateCheck.enabled = false;
    settings = {
      branding.appName = "thalheim.io mail";
      jmapServerUrl = "https://${stalwartHost}";
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

  services.nginx.virtualHosts.${vhost} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:8643";
      proxyWebsockets = true;
      extraConfig = ''
        client_max_body_size 256M;
      '';
    };
  };
}
