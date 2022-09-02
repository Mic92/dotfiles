{ pkgs, config, ... }: {
  services.nginx.virtualHosts."eva.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:3839";
      proxyWebsockets = true;
      extraConfig = ''
        keepalive_requests          100000;
        keepalive_timeout           160s;
        proxy_buffering             off;
        proxy_connect_timeout       75;
        proxy_ignore_client_abort   on;
        proxy_read_timeout          900s;
        proxy_send_timeout          600;
        send_timeout                600;
      '';
      priority = 99;
    };
  };
  systemd.services.derper = {
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      StateDirectory = "derper";
      ExecStart = "${pkgs.tailscale}/bin/derper -a :3839 --certdir /var/lib/derper --hostname=eva.thalheim.io --verify-clients";
    };
  };
}
