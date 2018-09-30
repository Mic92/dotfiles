{
  services.nginx = {
    virtualHosts."git.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3000;
      '';
    };
    virtualHosts."git.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "git.thalheim.io";
    };
  };
}
