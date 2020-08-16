{ pkgs, config, ... }: {
  services.squid = {
    enable = true;
    # We cannot disable the plain text port atm, but the firewall blocks it
    proxyPort = 8888;
    extraConfig = ''
      auth_param basic program ${pkgs.squid}/libexec/basic_ldap_auth -b "ou=users,dc=eve" -f "(&(objectClass=proxyUser)(mail=%s))" -D cn=squid,ou=system,ou=users,dc=eve -W ${config.sops.secrets.squid-ldap.path} -h 127.0.0.1
      acl ldapauth proxy_auth REQUIRED
      http_access allow ldapauth

      https_port 8889 cert=/var/lib/acme/devkid.net/fullchain.pem key=/var/lib/acme/devkid.net/key.pem

      # for netdata
      access_log stdio:/var/log/squid/access.log combined

      # for netdata
      http_access allow localhost manager
      http_access deny manager
    '';
  };


  networking.firewall.allowedTCPPorts = [ 8889 ];

  users.users.squid.extraGroups = [ "keys" ];
  systemd.services.squid.serviceConfig.SupplementaryGroups = [ "keys" ];

  sops.secrets.squid-ldap.owner = "squid";

  environment.etc."netdata/python.d/squid.conf".text = ''
    tcp8888new:
      name : 'local'
      host : 'localhost'
      port : 8888
      request : '/squid-internal-mgr/counters'
  '';

  environment.etc."netdata/python.d/web_log.conf".text = ''
    squid_log3:
      name: 'squid'
      path: '/var/log/squid/access.log'
  '';

  users.users.netdata.extraGroups = [ "squid" ];

  services.openldap.settings.children."cn={1}squid,cn=schema".attrs =  {
    cn = "{1}squid";
    objectClass = "olcSchemaConfig";
    olcObjectClasses = [
    ''(1.3.6.1.4.1.16548.1.2.4 NAME 'proxyUser'
        SUP top AUXILIARY
        DESC 'Account to allow a user to use the Squid proxy'
        MUST ( mail $ userPassword ))
    ''];
  };
}
