{ pkgs, ...}: 
{
  services.weechat.enable = true;
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

    virtualHosts."lekwati.com" = {
      useACMEHost = "lekwati.com";
      forceSSL = true;
    };

    virtualHosts."blog.lekwati.com" = {
      useACMEHost = "lekwati.com";
      forceSSL = true;
    };

    virtualHosts."glowing-bear.lekwati.com" = {
      useACMEHost = "lekwati.com";
      forceSSL = true;
      root = pkgs.glowing-bear;
      locations."/weechat".extraConfig = ''
        proxy_pass  http://localhost:4243/weechat;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 12h;
        proxy_send_timeout 12h;
      '';
    };
  };
}

# Custom css
#
#@import url('https://fonts.googleapis.com/css?family=Lato');
#body { font-family: 'Lato', sans-serif; }
#tr.bufferline { line-height: 1.4; }
#
#.repeated-prefix span {
#    position: relative;
#    left: -1000px;
#}
#.repeated-prefix span.hidden-bracket {
#    position: absolute;
#    left: -1000px;
#}
#
#.repeated-prefix span:nth-last-of-type(2):after {
#    content:"↪";
#    position: relative;
#    left: 1000px;
#}
#
#.repeated-prefix span.highlight {
#    position: initial;
#    left: initial;
#}
#
#.repeated-prefix span.hidden-bracket:after,
#.repeated-prefix span.highlight:after {
#    content: initial;
#    position: initial;
#    left: initial;
#}
