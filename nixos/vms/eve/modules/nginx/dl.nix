let 
  site = acmeHost: root: {
    useACMEHost = acmeHost;
    forceSSL = true;
    root = "/var/www/${root}";
    locations."/files/".extraConfig = ''
      internal;
      secure_link $arg_st;
      include /run/keys/nginx-secure-link;

      if ($secure_link = "") { return 403; }
      if ($secure_link = "0") { return 403; }
    '';
    locations."/".extraConfig = ''
      rewrite /([a-zA-Z0-9_\-]*)/(.*)$ /$2?st=$1;
    '';
  };
in {
  services.nginx = {
    virtualHosts."dl.devkid.net" = site "devkid.net" "dl.devkid.net";
    virtualHosts."dl.thalheim.io" = site "thalheim.io" "dl.thalheim.io";
    virtualHosts."dl.higgsboson.tk" = site "higgsboson.tk" "dl.higgsboson.tk";
  };

  krops.secrets.files.nginx-secure-link.owner = "nginx";

  services.netdata.httpcheck.checks."dl.thalheim.io" = {
    url = "https://dl.thalheim.io/OtNjoZOUnEn3H6LJZ1qcIw/test";
  };
}
