{ pkgs, config, ... }: {

  services.grafana = {
    enable = true;
    domain = "grafana.thalheim.io";
    rootUrl = "https://grafana.thalheim.io";
    analytics.reporting.enable = false;
    auth.anonymous = {
      enable = true;
      org_name = "c3d2";
    };
    extraOptions = {
      SERVER_ENFORCE_DOMAIN = "true";
      AUTH_LDAP_ENABLED = "true";
      AUTH_LDAP_CONFIG_FILE = "/run/grafana/ldap.toml";
    };
    smtp = {
      host = "mail.higgsboson.tk:587";
      user = "grafana@thalheim.io";
      passwordFile = "/run/keys/grafana-smtp-password";
      fromAddress = "grafana@thalheim.io";
    };
    database = {
      type = "postgres";
      name = "grafana";
      host = "172.23.75.10";
      user = "grafana";
      passwordFile = "/run/keys/grafana-db-password";
    };
    security.adminPasswordFile = "/run/keys/grafana-admin-password";
    addr = "0.0.0.0";
    port = 3001;
  };

  users.users.grafana.extraGroups = [ "keys" ];
  systemd.services.grafana = {
    serviceConfig = {
      SupplementaryGroups = [ "keys" ];
      RuntimeDirectory = ["grafana"];
    };
    preStart = let
      ldap = pkgs.writeTextFile {
        name = "ldap.toml";
        text = ''
          [[servers]]
          host = "172.23.75.3"
          port = 389
          bind_dn = "cn=grafana,ou=system,ou=users,dc=eve"
          bind_password = "@bindPassword@"
          search_filter = "(&(objectClass=grafana)(|(mail=%s)(uid=%s)))"
          search_base_dns = ["ou=users,dc=eve"]

          [servers.attributes]
          name = "givenName"
          surname = "sn"
          username = "uid"
          email =  "mail"
        '';
      };
    in ''
      umask 077 
      sed -e "s/@bindPassword@/$(cat /run/keys/grafana-ldap-password)/" ${ldap} > /run/grafana/ldap.toml
    '';
  };

  deployment.keys = {
    "grafana-smtp-password" = {
      keyFile = ../secrets/grafana-smtp-password;
      user = "grafana";
    };
    "grafana-db-password" = {
      keyFile = ../secrets/grafana-db-password;
      user = "grafana";
    };
    "grafana-admin-password" = {
      keyFile = ../secrets/grafana-admin-password;
      user = "grafana";
    };
    "grafana-ldap-password" = {
      keyFile = ../secrets/grafana-ldap-password;
      user = "grafana";
    };
  };
}
