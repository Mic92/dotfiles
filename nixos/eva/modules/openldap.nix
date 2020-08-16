{ config, lib, pkgs, ... }:

{
  services.openldap = {
    enable = true;
    suffix = "dc=eve";
    rootdn = "cn=admin,dc=eve";
    rootpwFile = config.sops.secrets.openldap-rootpw.path;
    extraConfig = ''
      access to attrs=userPassword
        by self         write
        by anonymous    auth
        by *            none
      access to attrs=loginShell
        by self write
        by * read
      access to *
        by * read

      syncrepl rid=123
        provider=ldap://eve.r:389
        type=refreshOnly
        interval=01:00:00:00
        searchbase="dc=eve"
        filter="*"
        schemachecking=off
        bindmethod=simple
        binddn="cn=ldapsync,ou=system,ou=users,dc=eve"
        include ${config.sops.secrets.ldapsync-password.path}
    '';
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
