{ pkgs, ... }:
{
  services.rtorrent.enable = true;
  services.rtorrent.user = "joerg";
  services.rtorrent.group = "users";
  services.rtorrent.dataDir = "/data/torrent";
  services.rtorrent.dataPermissions = "0755";
  services.rtorrent.port = 50000;
  services.rtorrent.configText = ''
    schedule2 = watch_start, 10, 10, ((load.start, (cat, (cfg.watch), "start/*.torrent")))
    schedule2 = watch_load, 11, 10, ((load.normal, (cat, (cfg.watch), "load/*.torrent")))

    # Bind to specific address to prevent multiple connections per torrent
    network.bind_address.set = 0.0.0.0
  '';
  services.rtorrent.openFirewall = true;

  # Warez nginx configuration
  services.nginx = {
    package = pkgs.nginxQuic.override {
      modules = [
        pkgs.nginxModules.pam
        pkgs.nginxModules.fancyindex
      ];
    };
    virtualHosts."warez.r" = {
      # TODO
      #enableACME = true;
      #addSSL = true;
      root = "/data/torrent/download";
      locations."/".extraConfig = ''
        fancyindex on;              # Enable fancy indexes.
        fancyindex_exact_size off;  # Output human-readable file sizes.
      '';
    };
  };
}
