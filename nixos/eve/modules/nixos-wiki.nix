{ config, ... }: {
  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = "nixos-wiki.thalheim.io";
    passwordFile = config.sops.secrets."nixos-wiki".path;
    extraConfig = ''
      $wgGroupPermissions['*']['createaccount'] = false;
    '';
  };

  sops.secrets."nixos-wiki".owner = config.services.phpfpm.pools.mediawiki.user;

  services.nginx.virtualHosts."nixos-wiki.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
  };
}
