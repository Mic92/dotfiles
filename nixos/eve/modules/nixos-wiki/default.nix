{ config, pkgs, ... }:
let
  hostname = "nixos-wiki.thalheim.io";
in
{
  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = hostname;
    passwordFile = config.sops.secrets."nixos-wiki".path;

    extensions.SyntaxHighlight_GeSHi = null; # provides <SyntaxHighlight> tags
    extensions.ParserFunctions = null;
    extensions.Cite = null;
    extensions.VisualEditor = null;
    extensions.ConfirmEdit = null;  # Combat SPAM with a simple Captcha
    extensions.StopForumSpam = pkgs.fetchzip {
      url = "https://extdist.wmflabs.org/dist/extensions/StopForumSpam-REL1_40-71b57ba.tar.gz";
      hash = "sha256-g8v4zr11c2e4bY0BNipJ48miyAF4WTNvlSMgb/NxPBA=";
    };

    extraConfig = ''
      # Disable account creation globally
      $wgGroupPermissions['*']['createaccount'] = false;

      # Disable anonymous editing
      $wgGroupPermissions['*']['edit'] = false;

      # Allow svg upload
      $wgFileExtensions[] = 'svg';
      $wgSVGConverterPath = "${pkgs.imagemagick}/bin";

      # Pretty URLs
      $wgUsePathInfo = true;

      # cache pages with APCu
      $wgMainCacheType = CACHE_ACCEL;

      # TODO: nixos favicon
      #$wgFavicon = "/favicon.ico";
      $wgDefaultSkin = 'vector-2022';
      # configure logos for vector-2022: https://www.mediawiki.org/wiki/Manual:$wgLogos
      $wgLogos = [
        '1x' => '/nixos.png',	
        'icon' => '/nixos.png',	
      ];

      # Combat SPAM with IP-Blocklists (StopForumSpam extension)
      $wgEnableDnsBlacklist = true;
      $wgDnsBlacklistUrls = array(
        'dnsbl.dronebl.org'
      );

      # required for fancy VisualEditor extension
      $wgGroupPermissions['user']['writeapi'] = true;

      # Enable content security policy
      $wgCSPHeader = true;

      # Disallow framing
      $wgEditPageFrameOptions = "DENY";

      $wgEnableEmail = true;
      $wgAllowHTMLEmail = false;
      $wgEmergencyContact = "nixos-wiki-emergency@thalheim.io";
      $wgPasswordSender   = "nixos-wiki@thalheim.io";           # Default FROM address
      $wgNoReplyAddress   = "nixos-wiki-no-reply@thalheim.io";  # Default Reply-To address
    '';
  };

  sops.secrets."nixos-wiki".owner = config.services.phpfpm.pools.mediawiki.user;

  services.nginx.virtualHosts.${config.services.mediawiki.nginx.hostName} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."=/nixos.png".alias = ./nixos.png;
  };
}
