{ config, lib, pkgs, ... }:

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

  imports = [
    ../../modules/openldap
  ];

  environment.etc."netdata/python.d/openldap.conf" = {
    source = config.sops.secrets.netdata-openldap-password.path;
    user = "netdata";
  };
  sops.secrets.netdata-openldap-password.owner = "netdata";
  sops.secrets.ldap-login = {};
  services.netdata.portcheck.checks.openldap.port = 389;
}
