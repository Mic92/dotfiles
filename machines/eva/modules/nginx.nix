{
  config,
  lib,
  ...
}:
let
  autheliaAuth = ''
    auth_request /authelia;
    auth_request_set $user $upstream_http_remote_user;
  '';

  autheliaLocation = ''
    internal;
    proxy_pass http://127.0.0.1:9091/api/verify;

    proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
    proxy_set_header X-Original-Method $request_method;
    proxy_set_header X-Forwarded-Method $request_method;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-Host $http_host;
    proxy_set_header X-Forwarded-Uri $request_uri;
    proxy_set_header X-Forwarded-For $remote_addr;
    proxy_set_header Content-Length "";
    proxy_set_header Connection "";

    proxy_pass_request_body off;

    proxy_connect_timeout 5s;
    proxy_send_timeout 5s;
    proxy_read_timeout 5s;
  '';
in
{
  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule { config.quic = true; });
  };

  config = {
    networking.firewall.allowedUDPPorts = [ 443 ]; # quic

    security.acme.certs."prometheus.r".server = config.retiolum.ca.acmeURL;
    security.acme.certs."alertmanager.r".server = config.retiolum.ca.acmeURL;

    services.nginx = {
      commonHttpConfig = ''
        add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains; preload' always;
      '';

      upstreams = {
        "@prometheus".extraConfig = "server localhost:9090;";
        "@alertmanager".extraConfig = "server localhost:9093;";
      };

      virtualHosts."prometheus.thalheim.io" = {
        forceSSL = true;
        enableACME = true;
        extraConfig = ''
          error_page 401 =302 https://auth-eva.thalheim.io/?rd=$scheme://$http_host$request_uri;
        '';
        locations."/" = {
          proxyPass = "http://@prometheus/";
          extraConfig = autheliaAuth;
        };
        locations."/authelia".extraConfig = autheliaLocation;
      };

      virtualHosts."prometheus.r" = {
        enableACME = true;
        addSSL = true;
        locations."/".proxyPass = "http://@prometheus/";
      };

      virtualHosts."alertmanager.thalheim.io" = {
        forceSSL = true;
        enableACME = true;
        extraConfig = ''
          error_page 401 =302 https://auth-eva.thalheim.io/?rd=$scheme://$http_host$request_uri;
        '';
        locations."/" = {
          proxyPass = "http://@alertmanager/";
          extraConfig = autheliaAuth;
        };
        locations."/authelia".extraConfig = autheliaLocation;
      };

      virtualHosts."alertmanager.r" = {
        enableACME = true;
        addSSL = true;
        locations."/".proxyPass = "http://@alertmanager/";
      };

      virtualHosts."ip2.thalheim.io" = {
        enableACME = true;
        addSSL = true;
        locations."/".extraConfig = ''
          default_type text/plain;
          return 200 "$remote_addr\n";
        '';
      };
    };
  };
}
