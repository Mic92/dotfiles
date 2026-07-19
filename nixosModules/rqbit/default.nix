{ ... }:
let
  autheliaLocation = {
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
  autheliaAuth = ''
    # Authelia forward-auth
    auth_request /authelia;
    auth_request_set $user $upstream_http_remote_user;
    error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;
  '';
in
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

  # Allow nginx (warez.thalheim.io) to read downloaded files
  users.users.nginx.extraGroups = [ "rqbit" ];

  # Media browser/player over downloads (Authelia protected).
  # Small static webapp optimized for iPad and Android TV.
  services.nginx.virtualHosts."warez.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "${./webapp}";

    locations."/authelia" = autheliaLocation;

    locations."/".extraConfig = ''
      ${autheliaAuth}
    '';

    # Directory listings as JSON for the webapp + range requests for streaming
    locations."/files/" = {
      alias = "/var/lib/rqbit/downloads/";
      extraConfig = ''
        ${autheliaAuth}
        autoindex on;
        autoindex_format json;
      '';
    };
  };

  # Web UI (rqbit ships one on its HTTP API port)
  services.nginx.virtualHosts."torrent.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;

    locations."/authelia" = autheliaLocation;

    locations."/".extraConfig = ''
      ${autheliaAuth}
      proxy_pass http://127.0.0.1:3030;
    '';
  };
}
