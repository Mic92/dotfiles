{pkgs, ...}: {
  services.nginx = {
    virtualHosts."threema.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "${pkgs.nur.repos.mic92.threema-web}";
    };
  };
}
