{ config, ... }: {
  services.harmonia.enable = true;
  services.harmonia.signKeyPath = config.sops.secrets.harmonia-key.path;

  nix.settings.allowed-users = [ "harmonia" ];

  services.nginx.virtualHosts."cache.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:5000;
      proxy_set_header Host $host;
      proxy_redirect http:// https://;
      proxy_http_version 1.1;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;

      zstd on;
      zstd_types application/x-nix-archive;
    '';
  };
}
