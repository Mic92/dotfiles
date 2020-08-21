{
  imports = [
    ../../modules/nginx.nix
  ];
  services.nginx = {
    upstreams = {
      "@prometheus".extraConfig = ''
        server localhost:9090;
      '';
      "@alertmanager".extraConfig = ''
        server localhost:9093;
      '';
    };
    virtualHosts."prometheus.thalheim.io" = {
      forceSSL = true;
      enableACME = true;
      locations."/".extraConfig = ''
        #auth_pam "Ldap password";
        #auth_pam_service_name "nginx";
      '';
    };
  };
}
