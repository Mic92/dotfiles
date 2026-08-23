{
  imports = [
    ../../../nixosModules/lldap
    ../../../nixosModules/lldap/sync-primary.nix
  ];

  services.lldap.settings.http_url = "https://lldap.thalheim.io";

  services.nginx.virtualHosts."lldap.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = "proxy_pass http://localhost:17170;";
  };
}
