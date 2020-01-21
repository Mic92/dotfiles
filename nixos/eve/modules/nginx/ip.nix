{
  services.nginx = {
    virtualHosts."ip.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        default_type text/plain;
        return 200 "$remote_addr\n";
      '';
    };
    virtualHosts."ip.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "ip.thalheim.io";
    };
  };
}
