{ pkgs
, config
, ...
}: {
  services.squid = {
    enable = true;
    # We cannot disable the plain text port atm, but the firewall blocks it
    proxyPort = 8800;
    extraConfig = ''
      auth_param basic program ${pkgs.squid}/libexec/basic_ldap_auth -b "ou=users,dc=eve" -f "(&(objectClass=proxyUser)(mail=%s))" -D cn=squid,ou=system,ou=users,dc=eve -W ${config.sops.secrets.squid-ldap.path} -h 127.0.0.1
      acl ldapauth proxy_auth REQUIRED
      http_access allow ldapauth

      https_port 8889 cert=/var/lib/acme/thalheim.io/fullchain.pem key=/var/lib/acme/thalheim.io/key.pem

      cache_log       /dev/null
      access_log      none
      cache_store_log /dev/null
    '';
  };

  networking.firewall.allowedTCPPorts = [ 8889 ];

  sops.secrets.squid-ldap.owner = "squid";
}
