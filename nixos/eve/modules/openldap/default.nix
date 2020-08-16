{ pkgs, lib, config, ... }:
{
  users.ldap = {
    enable = true;
    server = "ldap://127.0.0.1/";
    base = "ou=users,dc=eve";
    daemon.enable = true;
    bind = {
      distinguishedName = "cn=login,ou=system,ou=users,dc=eve";
      passwordFile = config.sops.secrets.ldap-login.path;
    };
  };

  services.openldap = {
    enable = true;

    defaultSchemas = null;
    dataDir = null;
    database = null;

    settings.children."cn=schema".includes = [
      "${pkgs.openldap}/etc/schema/core.ldif"
      "${pkgs.openldap}/etc/schema/cosine.ldif"
      "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
      "${pkgs.openldap}/etc/schema/nis.ldif"
    ];

    settings.children."olcDatabase={1}mdb" = {
      attrs = {
        objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
        olcDatabase = "{1}mdb";
        olcDbDirectory = "/var/db/openldap";
        olcRootPW.path = config.sops.secrets.openldap-rootpw.path;
        olcRootDN = "cn=admin,dc=eve";
        olcSuffix = "dc=eve";
        olcAccess = [
          ''{0}to attrs=userPassword
               by self write  by anonymous auth
               by dn.base="cn=dovecot,dc=mail,dc=eve" read
               by dn.base="cn=gitlab,ou=system,ou=users,dc=eve" read
               by dn.base="cn=ldapsync,ou=system,ou=users,dc=eve"
               read by * none''
          ''{1}to attrs=loginShell  by self write  by * read''
          ''{2}to dn.subtree="ou=system,ou=users,dc=eve"
               by dn.base="cn=dovecot,dc=mail,dc=eve" read
               by dn.subtree="ou=system,ou=users,dc=eve" read
               by * none''
          ''{3}to dn.subtree="ou=jabber,ou=users,dc=eve"  by dn.base="cn=prosody,ou=system,ou=users,dc=eve" write  by * read''
          ''{4}to * by * read''
        ];
      };
    };
    settings.children."olcDatabase={2}monitor".attrs = {
      olcDatabase = "{2}monitor";
      objectClass = [ "olcDatabaseConfig" "olcMonitorConfig" ];
      olcAccess = [''{0}to *
             by dn.exact="cn=netdata,ou=system,ou=users,dc=eve" read
             by * none''];
    };
  };

  sops.secrets.openldap-rootpw = {
    owner = "openldap";
    sopsFile = ../../../secrets/ldap.yaml;
  };

  environment.etc."netdata/python.d/openldap.conf" = {
    source = config.sops.secrets.netdata-openldap-password.path;
    user = "netdata";
  };
  sops.secrets.netdata-openldap-password.owner = "netdata";

  sops.secrets.ldap-login = {};

  users.users.openldap.extraGroups = [ "keys" ];
  systemd.services.openldap.serviceConfig.SupplementaryGroups = [ "keys" ];
  services.netdata.portcheck.checks.openldap.port = 389;
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 389 ];
}
