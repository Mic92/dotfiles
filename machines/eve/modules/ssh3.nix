{ config, lib, ... }:

{
  imports = [ ../../../nixosModules/ssh3.nix ];

  services.ssh3 = {
    enable = true;
    useACMEHost = "thalheim.io";
    urlPath = lib.mkDefault "ssh3-${
      builtins.substring 0 16 (builtins.hashString "sha256" "eve-ssh3-secret")
    }";
  };

  # Nginx reverse proxy configuration
  services.nginx.virtualHosts."ssh3-eve.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";

    locations."/" = {
      return = "404";
    };

    locations."/${config.services.ssh3.urlPath}" = {
      proxyPass = "https://127.0.0.1:${toString config.services.ssh3.port}";
      proxyWebsockets = true;
      extraConfig = ''
        # QUIC/HTTP3 specific settings
        proxy_http_version 3;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Disable buffering for QUIC
        proxy_buffering off;
        proxy_request_buffering off;

        # Increase timeouts for SSH sessions
        proxy_connect_timeout 7d;
        proxy_send_timeout 7d;
        proxy_read_timeout 7d;
      '';
    };

    # Enable HTTP/3
    http3 = true;
    quic = true;
    extraConfig = ''
      add_header Alt-Svc 'h3=":443"; ma=86400';
    '';
  };
}
