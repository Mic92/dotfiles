{ pkgs, config, ... }:
let
  ldap = pkgs.writeTextFile {
    name = "ldap.toml";
    text = ''
      [[servers]]
      host = "127.0.0.1"
      port = 3890
      bind_dn = "uid=grafana,ou=people,dc=eve"
      bind_password = "$__file{${config.clan.core.vars.generators.lldap-grafana.files.bind-password.path}}"
      search_filter = "(&(memberOf=cn=grafana,ou=groups,dc=eve)(|(mail=%s)(uid=%s)))"
      search_base_dns = ["ou=people,dc=eve"]

      [servers.attributes]
      name = "cn"
      username = "uid"
      email = "mail"
      member_of = "memberOf"

      # LDAP sync removes memberships of orgs without a mapping,
      # so list every org for grafana-admins explicitly.
      [[servers.group_mappings]]
      group_dn = "cn=grafana-admins,ou=groups,dc=eve"
      org_role = "Admin"
      org_id = 1 # Main Org.
      grafana_admin = true

      [[servers.group_mappings]]
      group_dn = "cn=grafana-admins,ou=groups,dc=eve"
      org_role = "Admin"
      org_id = 2 # devkid.net

      [[servers.group_mappings]]
      group_dn = "cn=grafana-admins,ou=groups,dc=eve"
      org_role = "Admin"
      org_id = 4 # eve

      [[servers.group_mappings]]
      group_dn = "cn=grafana-admins,ou=groups,dc=eve"
      org_role = "Admin"
      org_id = 6 # tincr

      [[servers.group_mappings]]
      group_dn = "*"
      org_role = "Viewer"
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
        password = "$__file{${config.clan.core.vars.generators.lldap-grafana.files.bind-password.path}}";
        fromAddress = "grafana@thalheim.io";
      };

      database = {
        type = "postgres";
        name = "grafana";
        host = "/run/postgresql";
        user = "grafana";
      };

      security = {
        admin_password = "$__file{${config.sops.secrets.grafana-admin-password.path}}";
        secret_key = "$__file{${config.clan.core.vars.generators.grafana.files.secret-key.path}}";
      };
    };
  };

  clan.core.vars.generators.lldap-grafana = {
    files.bind-password.owner = "grafana";
    runtimeInputs = [
      pkgs.openssl
      pkgs.coreutils
    ];
    script = ''
      openssl rand -base64 24 | tr -d '\n' > "$out/bind-password"
    '';
  };

  clan.core.vars.generators.grafana = {
    files.secret-key = {
      owner = "grafana";
    };
    runtimeInputs = [
      pkgs.openssl
      pkgs.coreutils
    ];
    script = ''
      openssl rand -hex 32 > "$out"/secret-key
    '';
  };

  services.nginx.virtualHosts."grafana.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = "proxy_pass http://localhost:3001;";
  };

  sops.secrets = {
    grafana-admin-password.owner = "grafana";
    # Only used for SMTP auth against the grafana mail account.
  };
}
