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
  };
}
