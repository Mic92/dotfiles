{ pkgs, config, ... }: let
  ldap = pkgs.writeTextFile {
    name = "ldap.toml";
    text = ''
      [[servers]]
      host = "127.0.0.1"
      port = 389
      bind_dn = "cn=grafana,ou=system,ou=users,dc=eve"
      bind_password = "$__file{/run/secrets/grafana-ldap-password}"
      search_filter = "(&(objectClass=grafana)(|(mail=%s)(uid=%s)))"
      search_base_dns = ["ou=users,dc=eve"]

      [servers.attributes]
      name = "givenName"
      surname = "sn"
      username = "uid"
      email =  "mail"
    '';
  };
in {
  services.grafana = {
    enable = true;
    domain = "grafana.thalheim.io";
    rootUrl = "https://grafana.thalheim.io";
    analytics.reporting.enable = false;
    extraOptions = {
      SERVER_ENFORCE_DOMAIN = "true";
      AUTH_LDAP_ENABLED = "true";
      AUTH_LDAP_CONFIG_FILE = ldap;

      AUTH_ANONYMOUS_ENABLED = "true";
      AUTH_ANONYMOUS_ORG_NAME = "Main Org.";
      AUTH_ANONYMOUS_ORG_ROLE = "Viewer";
    };
    smtp = {
      enable = true;
      host = "mail.thalheim.io:587";
      user = "grafana@thalheim.io";
      passwordFile = config.sops.secrets.grafana-ldap-password.path;
      fromAddress = "grafana@thalheim.io";
    };
    database = {
      type = "postgres";
      name = "grafana";
      host = "/run/postgresql";
      user = "grafana";
    };
    security.adminPasswordFile = config.sops.secrets.grafana-admin-password.path;
    addr = "0.0.0.0";
    port = 3001;
  };

  users.users.grafana.extraGroups = [ "keys" ];
  systemd.services.grafana.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.nginx = {
    virtualHosts."grafana.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3001;
      '';
    };
  };

  sops.secrets = {
    grafana-admin-password.owner = "grafana";
    grafana-ldap-password.owner = "grafana";
  };
}
