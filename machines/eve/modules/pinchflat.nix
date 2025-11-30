{
  config,
  pkgs,
  ...
}:
let
  hostname = "pinchflat.thalheim.io";
  downloadsDir = "/data/pinchflat";
  zfsDataset = "zroot/root/pinchflat";
in
{
  # Use the native NixOS Pinchflat service module
  services.pinchflat = {
    enable = true;
    port = 8945;
    mediaDir = downloadsDir;
    openFirewall = false; # We use nginx reverse proxy
    selfhosted = true; # Uses weak secret since we're behind Authelia
    extraConfig = {
      TZ = "Europe/Berlin";
      # Expose feed endpoints without Pinchflat's auth (we use Authelia instead)
      EXPOSE_FEED_ENDPOINTS = "true";
      # Use IPv6 for YouTube downloads (may help with rate limiting)
      ENABLE_IPV6 = "true";
    };
  };

  # Create ZFS dataset for pinchflat downloads before service starts
  systemd.services.pinchflat = {
    preStart = ''
      # Create ZFS dataset if it doesn't exist
      if ! ${pkgs.zfs}/bin/zfs list ${zfsDataset} >/dev/null 2>&1; then
        ${pkgs.zfs}/bin/zfs create -o mountpoint=${downloadsDir} ${zfsDataset}
      fi
      # Ensure correct ownership
      chown -R pinchflat:pinchflat ${downloadsDir}
    '';
    serviceConfig = {
      # Run preStart as root to create ZFS dataset
      PermissionsStartOnly = true;
    };
  };

  # Nginx reverse proxy with Authelia forward-auth
  services.nginx.virtualHosts.${hostname} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;

    # Authelia forward-auth verification endpoint (form-based login)
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
        proxy_set_header Content-Length "";
        proxy_pass_request_body off;
      '';
    };

    # Authelia basic auth endpoint (for podcast apps)
    locations."/authelia-basic" = {
      proxyPass = "http://127.0.0.1:9091/api/verify?auth=basic";
      extraConfig = ''
        internal;
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Method $request_method;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-Uri $request_uri;
        proxy_set_header X-Forwarded-For $remote_addr;
        # Pass Authorization header to Authelia for basic auth
        proxy_set_header Proxy-Authorization $http_authorization;
        proxy_set_header Content-Length "";
        proxy_pass_request_body off;
      '';
    };

    # Feed endpoints with Authelia HTTP basic auth (for podcast apps)
    # Match: /sources/opml, /sources/:uuid/feed, /sources/:uuid/feed_image
    locations."~ ^/sources/(opml|[^/]+/feed|[^/]+/feed_image)" = {
      proxyPass = "http://127.0.0.1:${toString config.services.pinchflat.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        # Use Authelia basic auth - triggers HTTP Basic Auth dialog
        auth_request /authelia-basic;
        auth_request_set $user $upstream_http_remote_user;
        proxy_set_header Remote-User $user;
      '';
    };

    # Media streaming endpoints with Authelia HTTP basic auth (for podcast apps)
    # Match: /media/:uuid/stream, /media/:uuid/episode_image
    locations."~ ^/media/[^/]+/(stream|episode_image)" = {
      proxyPass = "http://127.0.0.1:${toString config.services.pinchflat.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        # Use Authelia basic auth - triggers HTTP Basic Auth dialog
        auth_request /authelia-basic;
        auth_request_set $user $upstream_http_remote_user;
        proxy_set_header Remote-User $user;
      '';
    };

    # Main UI with Authelia forward-auth (form-based login)
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.pinchflat.port}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        # Redirect to Authelia login on 401
        error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;

        # Forward auth request to Authelia
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;

        # Pass authenticated username (Pinchflat doesn't use it, but good practice)
        proxy_set_header Remote-User $user;
      '';
    };
  };
}
