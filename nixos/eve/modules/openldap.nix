{ pkgs, lib, config, ... }:
{
  users.ldap = {
    enable = true;
    server = "ldap://127.0.0.1/";
    base = "ou=users,dc=eve";
    daemon.enable = true;
    bind = {
      distinguishedName = "cn=login,ou=system,ou=users,dc=eve";
      passwordFile = config.krops.secrets."ldap-login".path;
    };
  };

  services.openldap = {
    enable = true;
    suffix = "dc=eve";
    rootdn = "cn=admin,dc=eve";
    rootpwFile = config.krops.secrets."openldap-rootpw".path;
    extraConfig = ''
      access to attrs=userPassword
        by self         write
        by anonymous    auth
        by dn="cn=dovecot,dc=mail,dc=eve" read
        by dn="cn=gitlab,ou=system,ou=users,dc=eve" read
        by *            none
      access to attrs=loginShell
        by self write
        by * read
      access to dn.subtree="ou=system,ou=users,dc=eve"
        by dn="cn=dovecot,dc=mail,dc=eve" read
        by dn.subtree="ou=system,ou=users,dc=eve" read
        by * none
      access to dn.subtree="ou=jabber,ou=users,dc=eve"
        by dn="cn=prosody,ou=system,ou=users,dc=eve" write
        by * read
      access to *
        by * read

      database monitor
      access to *
        by dn.exact="cn=netdata,ou=system,ou=users,dc=eve" read
        by * none
   '';
  };

  environment.etc."netdata/python.d/openldap.conf" = {
    source = config.krops.secrets."netdata-openldap-password".path;
    user = "netdata";
  };
  krops.secrets.netdata-openldap-password.owner = "netdata";

  krops.secrets.openldap-rootpw.owner = "openldap";
  krops.secrets.ldap-login = {};

  users.users.openldap.extraGroups = [ "keys" ];
  systemd.services.openldap.serviceConfig.SupplementaryGroups = [ "keys" ];
  services.netdata.portcheck.checks.openldap.port = 389;
}
