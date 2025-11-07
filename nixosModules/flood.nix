{
  pkgs,
  lib,
  ...
}:
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

  # Nginx configuration
  services.nginx.virtualHosts."flood.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "${pkgs.flood}/lib/node_modules/flood/dist/assets";

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

    locations."/api".extraConfig = ''
      # Authelia forward-auth
      auth_request /authelia;
      auth_request_set $user $upstream_http_remote_user;
      error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;

      proxy_pass http://localhost:3003;
    '';

    locations."/".extraConfig = ''
      # Authelia forward-auth
      auth_request /authelia;
      auth_request_set $user $upstream_http_remote_user;
      error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;

      try_files $uri /index.html;
    '';
  };
}
