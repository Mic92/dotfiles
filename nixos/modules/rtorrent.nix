{ config
, pkgs
, ...
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
  services.rtorrent.enable = true;
  services.rtorrent.package = pkgs.jesec-rtorrent;
  services.rtorrent.user = "joerg";
  services.rtorrent.group = "users";
  services.rtorrent.dataDir = "/data/torrent";
  services.rtorrent.dataPermissions = "0755";
  services.rtorrent.configText = ''
    schedule2 = watch_start, 10, 10, ((load.start, (cat, (cfg.watch), "start/*.torrent")))
    schedule2 = watch_load, 11, 10, ((load.normal, (cat, (cfg.watch), "load/*.torrent")))
  '';
  services.rtorrent.openFirewall = true;

  security.pam.services.flood.text = ''
    auth required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
    account required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
  '';

  systemd.services.flood = {
    wantedBy = [ "multi-user.target" ];
    wants = [ "rtorrent.service" ];
    after = [ "rtorrent.service" ];
    serviceConfig = {
      User = "joerg";
      ExecStart = "${pkgs.nodePackages.flood}/bin/flood --auth none --port 3003 --rtsocket /run/rtorrent/rpc.sock";
    };
  };

  security.acme.certs."flood.r".server = config.retiolum.ca.acmeURL;

  services.nginx = {
    package = pkgs.nginxQuic.override {
      modules = [
        pkgs.nginxModules.pam
        pkgs.nginxModules.fancyindex
        pkgs.nginxModules.zstd
      ];
    };
    virtualHosts."flood.r" = {
      # TODO
      enableACME = true;
      addSSL = true;
      root = "${pkgs.nodePackages.flood}/lib/node_modules/flood/dist/assets";
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
    virtualHosts."warez.r" = {
      # TODO
      #enableACME = true;
      #addSSL = true;
      root = "/data/torrent/download";
      locations."/" .extraConfig = ''
        fancyindex on;              # Enable fancy indexes.
        fancyindex_exact_size off;  # Output human-readable file sizes.
      '';
    };
  };
}
