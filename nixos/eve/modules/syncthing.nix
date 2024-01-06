{
  services.syncthing = {
    enable = true;
    group = "users";
    guiAddress = "0.0.0.0:8384";
  };

  services.nginx = {
    virtualHosts."syncthing.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_set_header        Host $host;
        proxy_set_header        X-Real-IP $remote_addr;
        proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header        X-Forwarded-Proto $scheme;

        proxy_pass              http://localhost:8384/;

        proxy_read_timeout      600s;
        proxy_send_timeout      600s;
      '';
    };
  };

  fileSystems."/var/lib/syncthing/Sync/public" = {
    device = "/var/www/dl.lekwati.com";
    options = [ "bind" "nofail" ];
  };

  networking.firewall.allowedTCPPorts = [ 22000 ]; # tcp
  networking.firewall.allowedUDPPorts = [ 22000 ]; # quic
}
