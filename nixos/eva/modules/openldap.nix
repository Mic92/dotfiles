{ config, lib, pkgs, ... }:

{
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
        # Current value (must be stored completly in file)
        # provider=ldap://eve.r:389
        # type=refreshOnly
        # interval=01:00:00:00
        # searchbase="dc=eve"
        # schemachecking=off
        # bindmethod=simple
        # binddn="cn=ldapsync,ou=system,ou=users,dc=eve"
        # credentials=<secret>
        olcSyncRepl.path = config.sops.secrets.ldapsync-password.path;
      };
    };
  };

  sops.secrets.openldap-rootpw = {
    owner = "openldap";
    sopsFile = ../../secrets/ldap.yaml;
  };
  sops.secrets.ldapsync-password = {
    owner = "openldap";
  };
  systemd.services.openldap.serviceConfig.SupplementaryGroups = [ "keys" ];
}
