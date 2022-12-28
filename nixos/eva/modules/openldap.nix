{ config, ... }: {
  imports = [
    ../../modules/openldap
  ];

  services.openldap.settings.children."olcDatabase={1}mdb".attrs = {
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

  sops.secrets.ldapsync-password.owner = "openldap";
}
