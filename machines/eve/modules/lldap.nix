{ pkgs, config, ... }:
{
  imports = [
    ../../../nixosModules/lldap
    ../../../nixosModules/lldap/sync-primary.nix
    ../../../nixosModules/lldap/ensure.nix
    ../../../nixosModules/lldap/alertmanager-smtp.nix
  ];

  services.lldap.ensureUsers.alertmanager = {
    email = "alertmanager@thalheim.io";
    passwordFile = config.clan.core.vars.generators.alertmanager-smtp.files.password.path;
    groups = [ "mail" ];
  };

  # Service-specific groups live next to their service. prometheus is
  # consumed by authelia on eva, whose lldap is a read-only replica.
  services.lldap.ensureGroups = [
    "admins"
    "prometheus"
  ];

  services.lldap.ensureUsers.lldap = {
    email = "lldap@thalheim.io";
    passwordFile = config.clan.core.vars.generators.lldap-smtp.files.password.path;
    groups = [ "mail" ];
  };

  services.lldap.ensureUsers.authelia = {
    email = "authelia@thalheim.io";
    passwordFile = config.clan.core.vars.generators.lldap-authelia.files.bind-password.path;
    groups = [
      "lldap_password_manager"
      "mail"
    ];
  };

  services.lldap.settings.http_url = "https://lldap.thalheim.io";

  # Self-service password reset emails. Only on eve: eva's database is an
  # overwritten replica, resets there would be lost.
  clan.core.vars.generators.lldap-smtp = {
    files.password = { };
    files.env = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''
      openssl rand -base64 24 | tr -d '\n' > "$out/password"
      printf 'LLDAP_SMTP_OPTIONS__PASSWORD=%s' "$(cat "$out/password")" > "$out/env"
    '';
  };

  services.lldap.environment = {
    LLDAP_SMTP_OPTIONS__ENABLE_PASSWORD_RESET = "true";
    LLDAP_SMTP_OPTIONS__SERVER = "mail.thalheim.io";
    LLDAP_SMTP_OPTIONS__PORT = "587";
    LLDAP_SMTP_OPTIONS__SMTP_ENCRYPTION = "STARTTLS";
    LLDAP_SMTP_OPTIONS__USER = "lldap@thalheim.io";
    LLDAP_SMTP_OPTIONS__FROM = "LLDAP <lldap@thalheim.io>";
  };

  systemd.services.lldap.serviceConfig.EnvironmentFile = [
    config.clan.core.vars.generators.lldap-smtp.files.env.path
  ];

  services.nginx.virtualHosts."lldap.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = "proxy_pass http://localhost:17170;";
  };
}
