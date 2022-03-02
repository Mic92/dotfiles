{config, ...}: {
  services.navidrome.enable = true;
  services.navidrome.settings.MusicFolder = "/data/torrent/download";
  services.navidrome.settings.ReverseProxyWhitelist = "127.0.0.1/32";
  services.navidrome.settings.ScanSchedule = "@every 1h";

  security.acme.certs."navidrome.r".server = config.retiolum.ca.acmeURL;

  services.nginx.virtualHosts."navidrome.r" = {
    enableACME = true;
    addSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:4533;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_set_header X-Forwarded-Protocol $scheme;
      proxy_set_header X-Forwarded-Host $http_host;
      proxy_set_header Remote-User krebs;
    '';
  };

  services.nginx.virtualHosts."navidrome.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:4533;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_set_header X-Forwarded-Protocol $scheme;
      proxy_set_header X-Forwarded-Host $http_host;
      proxy_set_header Remote-User "";
    '';
  };
}
