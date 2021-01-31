{
  services.nginx = {
    virtualHosts."blog.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/blog.higgsboson.tk";
    };
  };
}
