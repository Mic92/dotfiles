{ config, pkgs, ... }:
{
  services.nextcloud = {
    enable = true;
    hostName = "cloud.thalheim.io";
    https = true;

    caching.apcu = true;

    package = pkgs.nextcloud32;

    config = {
      dbtype = "pgsql";
      dbname = "nextcloud";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      adminuser = "nextcloudadmin";
      adminpassFile = config.sops.secrets.nextcloud-admin-password.path;
    };

    settings.trusted_domains = [ "pim.devkid.net" ];
    settings.dbtableprefix = "oc_";

    poolSettings = {
      "pm" = "ondemand";
      "pm.max_children" = 32;
      "pm.process_idle_timeout" = "10s";
      "pm.max_requests" = 500;
    };
  };

  # LDAP configuration will be applied via occ commands
  systemd.services.nextcloud-ldap-config = {
    after = [ "nextcloud-setup.service" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      # Set LDAP configuration (using default configuration ID)
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapHost "127.0.0.1"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapPort "389"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapAgentName "cn=nextcloud,ou=system,ou=users,dc=eve"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapAgentPassword "$(cat ${config.sops.secrets.nextcloud-ldap-password.path})"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapBase "ou=users,dc=eve"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapBaseUsers "ou=users,dc=eve"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapUserFilter "(objectClass=ownCloud)"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapUserDisplayName "cn"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapEmailAttribute "mail"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapLoginFilter "(&(objectClass=ownCloud)(mail=%uid))"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapExpertUsernameAttr "mail"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapQuotaAttribute "owncloudquota"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapQuotaDefault "10000000000"
      ${config.services.nextcloud.occ}/bin/nextcloud-occ ldap:set-config "" ldapConfigurationActive "1"

      # Enable LDAP app
      ${config.services.nextcloud.occ}/bin/nextcloud-occ app:enable user_ldap || true
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "nextcloud";
      RemainAfterExit = true;
    };
  };

  sops.secrets.nextcloud-admin-password.owner = "nextcloud";
  sops.secrets.nextcloud-ldap-password.owner = "nextcloud";

  services.nginx.virtualHosts."cloud.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    serverAliases = [ "pim.devkid.net" ];
  };
}
