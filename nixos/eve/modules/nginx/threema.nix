{ config, ... }: {
  services.nginx = {
    virtualHosts."threema.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "${config.nur.repos.mic92.threema-web}";
    };
  };
}
