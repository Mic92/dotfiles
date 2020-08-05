{ lib, ... }: {
  services.nginx.virtualHosts."muc.thalheim.io" = {
    useACMEHost = lib.mkForce "thalheim.io";

    forceSSL = true;

    locations."/http-bind/".extraConfig = ''
      proxy_buffering off;
      tcp_nodelay on;
      keepalive_timeout 55;
      proxy_set_header Host thalheim.io;
      proxy_pass http://localhost:5280/http-bind;
    '';

    root = "/var/www/muc.thalheim.io";
  };
}
