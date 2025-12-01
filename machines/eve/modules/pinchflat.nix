let
  hostname = "pinchflat.thalheim.io";
  # Pinchflat runs on blob64, accessed via wireguard
  upstream = "http://blob64.x:8945";
in
{
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
        proxy_set_header Proxy-Authorization $http_authorization;
        proxy_set_header Content-Length "";
        proxy_pass_request_body off;
      '';
    };

    # Feed endpoints with Authelia HTTP basic auth (for podcast apps)
    # Match: /sources/opml.xml, /sources/:uuid/feed.xml, /sources/:uuid/feed_image
    locations."~ ^/sources/(opml|[^/]+/feed|[^/]+/feed_image)(\\.\\w+)?$" = {
      proxyPass = upstream;
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        auth_request /authelia-basic;
        auth_request_set $user $upstream_http_remote_user;
        proxy_set_header Remote-User $user;
      '';
    };

    # Media streaming endpoints with Authelia HTTP basic auth (for podcast apps)
    # Match: /media/:uuid/stream, /media/:uuid/episode_image
    locations."~ ^/media/[^/]+/(stream|episode_image)" = {
      proxyPass = upstream;
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        auth_request /authelia-basic;
        auth_request_set $user $upstream_http_remote_user;
        proxy_set_header Remote-User $user;
      '';
    };

    # Main UI with Authelia forward-auth (form-based login)
    locations."/" = {
      proxyPass = upstream;
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        error_page 401 =302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;
        proxy_set_header Remote-User $user;
      '';
    };
  };
}
