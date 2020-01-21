{
  services.nginx = {
    virtualHosts."blog.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/blog.higgsboson.tk";
    };
    virtualHosts."www.blog.thalheim.io" = {
      globalRedirect = "blog.thalheim.io";
    };
    virtualHosts."www.blog.higgsboson.tk" = {
      globalRedirect = "blog.thalheim.io";
    };
    virtualHosts."blog.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "blog.thalheim.io";
    };
  };
}
