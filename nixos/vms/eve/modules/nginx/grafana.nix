{
  services.nginx = {
    virtualHosts."grafana.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:3001;
      '';
    };
  };
}
