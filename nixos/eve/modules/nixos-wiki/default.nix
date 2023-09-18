{ config, pkgs, ... }:
let
  hostname = "nixos-wiki.thalheim.io";
  mediawiki-maintenance = pkgs.runCommand "mediawiki-maintenance"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
      preferLocalBuild = true;
    } ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.php}/bin/php $out/bin/mediawiki-maintenance \
      --set MEDIAWIKI_CONFIG ${config.services.phpfpm.pools.mediawiki.phpEnv.MEDIAWIKI_CONFIG} \
      --add-flags ${config.services.mediawiki.finalPackage}/share/mediawiki/maintenance/run.php
  '';

  wikiDump = "/var/backup/wikidump.xml.gz";

  wiki-restore = pkgs.writeShellApplication {
    name = "wiki-restore";
    runtimeInputs = [
      pkgs.postgresql
      pkgs.coreutils
      pkgs.util-linux
      mediawiki-maintenance
    ];
    text = ''
      tmpdir=$(mktemp -d)
      cleanup() { rm -rf "$tmpdir"; }
      cd "$tmpdir"
      chown mediawiki:mediawiki "$tmpdir"

      rm -rf /var/lib/mediawiki-uploads
      install -d -m 755 -o mediawiki -g mediawiki /var/lib/mediawiki-uploads
      runuser -u postgres -- dropdb mediawiki 
      systemctl stop phpfpm-mediawiki.service
      systemctl restart postgresql
      systemctl restart mediawiki-init.service
      #echo Main_Page | runuser -u mediawiki -- mediawiki-maintenance deleteBatch.php
      trap cleanup EXIT
      cp ${wikiDump} "$tmpdir"
      chown mediawiki:mediawiki "$tmpdir/wikidump.xml.gz"
      chmod 644 "$tmpdir/wikidump.xml.gz"
      runuser -u mediawiki -- mediawiki-maintenance importDump.php --uploads "$tmpdir/wikidump.xml.gz"
      runuser -u mediawiki -- mediawiki-maintenance rebuildrecentchanges.php
      systemctl start phpfpm-mediawiki.service
    '';
  };
in
{
  environment.systemPackages = [ mediawiki-maintenance ];

  systemd.services.wiki-backup = {
    startAt = "hourly";

    serviceConfig = {
      ExecStart = [
        "${pkgs.wget}/bin/wget https://nixos.wiki/images/wikidump.xml.gz -O ${wikiDump}.new"
        "${pkgs.coreutils}/bin/mv ${wikiDump}.new ${wikiDump}"
      ];
      Type = "oneshot";
    };
  };

  systemd.services.wiki-restore = {
    startAt = "daily";
    path = [ pkgs.postgresql mediawiki-maintenance ];

    serviceConfig = {
      ExecStart = "${wiki-restore}/bin/wiki-restore";
      Type = "oneshot";
    };
  };

  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = hostname;
    uploadsDir = "/var/lib/mediawiki-uploads";
    passwordFile = config.sops.secrets."nixos-wiki".path;

    extensions.SyntaxHighlight_GeSHi = null; # provides <SyntaxHighlight> tags
    extensions.ParserFunctions = null;
    extensions.Cite = null;
    extensions.VisualEditor = null;
    extensions.ConfirmEdit = null; # Combat SPAM with a simple Captcha
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
    locations."=/wikidump.xml.gz".alias = wikiDump;
  };
}
