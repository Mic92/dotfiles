{
  config,
  pkgs,
  lib,
  ...
}:
let
  conf = pkgs.writeText "ldap.conf" ''
    base dc=eve
    host localhost:389
    pam_login_attribute mail
    pam_filter objectClass=flood
  '';
in
{
  # Enable the NixOS flood service
  services.flood = {
    enable = true;
    port = 3003;
    host = "localhost";
    extraArgs = [
      "--auth=none"
      "--rtsocket=/run/rtorrent/rpc.sock"
    ];
  };

  # Override the systemd service to run as joerg user
  systemd.services.flood = {
    wants = [ "rtorrent.service" ];
    after = [ "rtorrent.service" ];
    serviceConfig = {
      User = "joerg";
      Group = "users";
      DynamicUser = lib.mkForce false;
      StateDirectory = "flood";
      StateDirectoryMode = "0755";
    };
  };

  # PAM configuration for flood authentication
  security.pam.services.flood.text = ''
    auth required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
    account required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
  '';

  # ACME certificate
  security.acme.certs."flood.r".server = config.retiolum.ca.acmeURL;

  # Nginx configuration
  services.nginx.virtualHosts."flood.r" = {
    enableACME = true;
    addSSL = true;
    root = "${pkgs.flood}/lib/node_modules/flood/dist/assets";
    locations."/api".extraConfig = ''
      auth_pam "Ldap password";
      auth_pam_service_name "flood";
      proxy_pass       http://localhost:3003;
    '';
    locations."/".extraConfig = ''
      auth_pam "Ldap password";
      auth_pam_service_name "flood";
      try_files $uri /index.html;
    '';
  };
}
