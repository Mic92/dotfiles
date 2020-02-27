{ pkgs, ... }:
let
  ldapConfig = {
    bitwarden_url = "https://bitwarden.thalheim.io";
    bitwarden_admin_token = "@ADMIN_TOKEN@";
    ldap_host = "localhost";
    ldap_bind_dn = "cn=bitwarden,ou=system,ou=users,dc=eve";
    ldap_bind_password = "@LDAP_PASSWORD@";
    ldap_search_base_dn = "ou=users,dc=eve";
    ldap_search_filter = "(&(objectClass=bitwarden))";
    ldap_sync_interval_seconds = 360;
  };

  ldapConfigFile = pkgs.runCommand "config.toml" {
    buildInputs = [ pkgs.remarshal ];
    preferLocalBuild = true;
  } ''
    remarshal -if json -of toml \
    < ${pkgs.writeText "config.json" (builtins.toJSON ldapConfig)} \
    > $out
  '';
in {
  services.bitwarden_rs = {
    enable = true;
    dbBackend = "postgresql";
    config = {
      domain = "https://bitwarden.thalheim.io";
      signupsAllowed = false;
      rocketPort = 3011;
      databaseUrl = "postgresql://bitwarden_rs@%2Frun%2Fpostgresql/bitwarden_rs";
      enableDbWal = "false";
      websocketEnabled = true;
      smtpHost = "mail.thalheim.io";
      smtpFrom = "bitwarden@thalheim.io";
      smtpUsername = "bitwarden@thalheim.io";
    };
  };

  systemd.services.bitwarden_rs.serviceConfig = {
    EnvironmentFile = ["/run/keys/bitwarden-smtp-password"];
    SupplementaryGroups = [ "keys" ];
  };

  systemd.services.bitwarden_ldap = {
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      sed \
        -e "s=@LDAP_PASSWORD@=$(</run/keys/bitwarden-ldap-password)=" \
        -e "s=@ADMIN_TOKEN@=$(</run/keys/bitwarden-admin-token)=" \
        ${ldapConfigFile} \
        > /run/bitwarden_ldap/config.toml
    '';

    serviceConfig = {
      ExecStart = "${pkgs.nur.repos.mic92.bitwarden_rs_ldap}/bin/bitwarden_rs_ldap";
      Environment = "CONFIG_PATH=/run/bitwarden_ldap/config.toml";

      SupplementaryGroups = [ "keys" ];
      RuntimeDirectory = [ "bitwarden_ldap" ];
      User = "bitwarden_ldap";
    };
  };

  services.nginx = {
    virtualHosts."bitwarden.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        client_max_body_size 128M;
      '';
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3011;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
      locations."/notifications/hub".extraConfig = ''
        proxy_pass http://localhost:3011;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
      locations."/notifications/hub/negotiate".extraConfig = ''
        proxy_pass http://localhost:3011;
      '';
    };
  };

  krops.secrets.files = {
    bitwarden-ldap-password.owner = "bitwarden_ldap";
    bitwarden-admin-token.owner = "bitwarden_ldap";
    bitwarden-smtp-password.owner = "bitwarden_rs";
  };

  users.users.bitwarden_ldap = {
    isSystemUser = true;
    group = "bitwarden_ldap";
  };

  users.groups.bitwarden_ldap = {};

  services.openldap.extraConfig = ''
    objectClass ( 1.3.6.1.4.1.28298.1.2.4 NAME 'bitwarden'
            SUP uidObject AUXILIARY
            DESC 'Added to an account to allow bitwarden access'
            MUST (mail $ userPassword) )
  '';

  services.icinga2.extraConfig = ''
    apply Service "Bitwarden v6 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "bitwarden.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Bitwarden v4 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "bitwarden.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
