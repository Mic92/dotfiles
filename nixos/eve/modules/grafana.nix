{ pkgs
, config
, ...
}:
let
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
in
{
  services.grafana = {
    enable = true;
    settings = {
      analytics.reporting_enabled = false;
      "auth.ldap".enabled = true;
      "auth.ldap".config_file = toString ldap;

      "auth.anonymous".enabled = true;
      "auth.anonymous".org_name = "Main Org.";
      "auth.anonymous".org_role = "Viewer";

      server = {
        root_url = "https://grafana.thalheim.io";
        domain = "grafana.thalheim.io";
        enforce_domain = true;
        enable_gzip = true;
        http_addr = "0.0.0.0";
        http_port = 3001;
      };

      smtp = {
        enabled = true;
        host = "mail.thalheim.io:587";
        user = "grafana@thalheim.io";
        password = "$__file{${config.sops.secrets.grafana-ldap-password.path}}";
        fromAddress = "grafana@thalheim.io";
      };

      database = {
        type = "postgres";
        name = "grafana";
        host = "/run/postgresql";
        user = "grafana";
      };

      security.admin_password = "$__file{${config.sops.secrets.grafana-admin-password.path}}";
    };
  };

  services.nginx.virtualHosts."grafana.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3001;";
  };

  sops.secrets = {
    grafana-admin-password.owner = "grafana";
    grafana-ldap-password.owner = "grafana";
  };
}
