{ pkgs, ... }: {

  services.squid = {
    enable = true;
    proxyPort = 8888;
    extraConfig = ''
      auth_param basic program ${pkgs.squid}/libexec/basic_ldap_auth -b "ou=users,dc=eve" -f "(&(objectClass=proxyUser)(mail=%s)(accountActive=TRUE)(delete=FALSE))" -D cn=squid,ou=system,ou=users,dc=eve -W /run/keys/squid-ldap -h 127.0.0.1
      acl ldapauth proxy_auth REQUIRED
      http_access allow ldapauth

      https_port 8889 cert=/var/lib/acme/devkid.net/fullchain.pem key=/var/lib/acme/devkid.net/key.pem
    '';
  };

  users.users.squid.extraGroups = [ "keys" ];
  systemd.services.squid.serviceConfig.SupplementaryGroups = [ "keys" ];

  deployment.keys = {
    "squid-ldap" = {
      keyFile = ../secrets/squid-ldap;
      user = "squid";
    };
  };
}
