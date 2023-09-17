{ config, ... }: {
  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = "nixos-wiki.thalheim.io";
    passwordFile = config.sops.secrets."nixos-wiki".path;
    extensions.SyntaxHighlight_GeSHi = null;
    extensions.ParserFunctions = null;
    extensions.Cite = null;

    extraConfig = ''
      $wgGroupPermissions['*']['createaccount'] = false;
      $wgMainCacheType = CACHE_ACCEL;
    '';
  };

  sops.secrets."nixos-wiki".owner = config.services.phpfpm.pools.mediawiki.user;

  services.nginx.virtualHosts."nixos-wiki.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
  };
}
