{ config, ... }: {
  sops.secrets."bing-gpt-cookies" = {};
  services.bing-gpt-server = {
    enable = true;
    port = 8683;
    cookieFile = config.sops.secrets."bing-gpt-cookies".path;
  };

  services.nginx = {
    enable = true;
    virtualHosts."bing-gpt.r".locations."/".proxyPass = "http://localhost:8683";
  };
}
