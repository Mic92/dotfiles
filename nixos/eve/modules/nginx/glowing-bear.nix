{ pkgs, ... }: {
  services.weechat.enable = true;
  services.nginx = {
    virtualHosts."glowing-bear.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."^~ /weechat" = {
        proxyPass = "http://127.0.0.1:4242";
        proxyWebsockets = true;
      };
      locations."/".root = pkgs.glowing-bear;
    };

    virtualHosts."lekwati.com" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
    };

    virtualHosts."glowing-bear.lekwati.com" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."^~ /weechat" = {
        proxyPass = "http://127.0.0.1:4243";
        proxyWebsockets = true;
      };
      locations."/".root = pkgs.glowing-bear;
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

