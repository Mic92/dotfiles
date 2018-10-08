{ pkgs, ... }: {
  services.nginx = {
    virtualHosts."www.threema.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "threema.thalheim.io";
    };

    virtualHosts."threema.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "${pkgs.nur.repos.joerg.threema-web}";
    };
  };
}
