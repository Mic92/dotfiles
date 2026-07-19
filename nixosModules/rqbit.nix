{ pkgs, ... }:
{
  services.rqbit = {
    enable = true;
    peerPort = 50000;
    openFirewall = true;
  };

  systemd.services.rqbit.environment = {
    # Private tracker: no DHT, no UPnP
    RQBIT_DHT_DISABLE = "true";
    RQBIT_UPNP_PORT_FORWARD_DISABLE = "true";
  };

  # Allow nginx (warez.r) to read downloaded files
  users.users.nginx.extraGroups = [ "rqbit" ];

  services.nginx.package = pkgs.nginx.override {
    modules = [
      pkgs.nginxModules.pam
      pkgs.nginxModules.fancyindex
    ];
  };

  # File browser over downloads
  services.nginx.virtualHosts."warez.r" = {
    root = "/var/lib/rqbit/downloads";
    locations."/".extraConfig = ''
      fancyindex on;              # Enable fancy indexes.
      fancyindex_exact_size off;  # Output human-readable file sizes.
    '';
  };

  # Web UI (rqbit ships one on its HTTP API port)
  services.nginx.virtualHosts."torrent.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;

    # Authelia endpoint configuration
    locations."/authelia" = {
      proxyPass = "http://127.0.0.1:9091/api/verify";
      extraConfig = ''
        internal;
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Method $request_method;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-Uri $request_uri;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_pass_request_body off;
        proxy_set_header Content-Length "";
      '';
    };

    locations."/".extraConfig = ''
      # Authelia forward-auth
      auth_request /authelia;
      auth_request_set $user $upstream_http_remote_user;
      error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;

      proxy_pass http://127.0.0.1:3030;
    '';
  };
}
