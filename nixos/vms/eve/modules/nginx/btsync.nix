{
  services.nginx = {
    virtualHosts."sync.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "sync.thalheim.io";
    };

    virtualHosts."sync.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://172.23.75.31:8888;
      '';
    };
  };
}
