{ config, ... }: let
  hostname = "nixos-wiki.thalheim.io";
in {
  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = hostname;
    passwordFile = config.sops.secrets."nixos-wiki".path;
    extensions.SyntaxHighlight_GeSHi = null;
    extensions.ParserFunctions = null;
    extensions.Cite = null;
    extensions.VisualEditor = null;

    extraConfig = ''
      $wgGroupPermissions['*']['createaccount'] = false;
      $wgMainCacheType = CACHE_ACCEL;
      $wgLogo = '/nixos.png';
      $wgDefaultSkin = 'vector-2022';
    '';
  };

  sops.secrets."nixos-wiki".owner = config.services.phpfpm.pools.mediawiki.user;

  services.nginx.virtualHosts.${config.services.mediawiki.nginx.hostName} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."=/nixos.png".alias = ./nixos.png;
  };
}
