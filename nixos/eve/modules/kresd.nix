{ pkgs, ... }: {
  services.kresd = {
    enable = true;
    listenDoH = [ "[::1]:8053" ];
    listenPlain = [
      "[::1]:53"
      "127.0.0.1:53"
    ];
    extraConfig = ''
      modules = { 'hints > iterate' }
      hints.add_hosts('${pkgs.retiolum}/etc.hosts')
    '';
  };

  # This causes services to fail on upgrade
  systemd.services."kresd@".restartIfChanged = false;

  networking.firewall.allowedTCPPorts = [ 853 ];

  services.nginx = {
    enableReload = true;
    streamConfig = ''
      upstream dns {
        zone dns 64k;
        server [::1]:53;
      }
      server {
        listen 853 ssl;
        ssl_certificate /var/lib/acme/thalheim.io/fullchain.pem;
        ssl_certificate_key /var/lib/acme/thalheim.io/key.pem;
        ssl_trusted_certificate /var/lib/acme/thalheim.io/fullchain.pem;
        proxy_pass dns;
      }
    '';
    upstreams."doh" = {
      extraConfig = ''
        zone doh 64k;
      '';
      servers."[::1]:8053" = {};
    };
    virtualHosts."dns.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      http2 = true;
      locations."/".extraConfig = ''
        return 404 "404 Not Found\n";
      '';
      locations."/dns-query".extraConfig = ''
        proxy_http_version 1.1;
        proxy_set_header Connection "";
        proxy_pass https://doh/doh;
        proxy_ssl_verify off;
      '';
    };
  };
}
