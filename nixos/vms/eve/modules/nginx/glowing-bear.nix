{ pkgs, ...}: 
{
  services.nginx = {
    virtualHosts."glowing-bear.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = pkgs.glowing-bear;
      locations."/weechat".extraConfig = ''
        proxy_pass  http://localhost:4242/weechat;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 12h;
        proxy_send_timeout 12h;
      '';
    };
  };

  services.netdata.httpcheck.checks.glowing-bear = {
    url = "https://glowing-bear.thalheim.io";
    regex = "Glowing";
  };
}
