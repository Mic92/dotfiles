{ config, ... }: {
  services.bing-gpt-server = {
    enable = true;
    port = 8683;
    cookieFile = config.sops.secrets."bing-gpt-cookies".path;
  };

  services.nginx = {
    enable = true;
    virtualHosts."bing-gpt.r".locations."/".proxyPass = "http://127.0.0.1:8683";
  };
}
