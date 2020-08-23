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
    '';
  };


  networking.firewall.allowedTCPPorts = [ 8889 ];

  users.users.squid.extraGroups = [ "keys" ];
  systemd.services.squid.serviceConfig.SupplementaryGroups = [ "keys" ];

  sops.secrets.squid-ldap.owner = "squid";
}
