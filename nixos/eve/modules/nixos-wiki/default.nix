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
      chown mediawiki:nginx "$tmpdir"

      rm -rf /var/lib/mediawiki-uploads
      install -d -m 755 -o mediawiki -g nginx /var/lib/mediawiki-uploads
      systemctl stop phpfpm-mediawiki.service
      runuser -u postgres -- dropdb mediawiki
      systemctl restart postgresql
      systemctl restart mediawiki-init.service
      cat <<EOF | runuser -u mediawiki -- mediawiki-maintenance deleteBatch.php
      Main_Page
      MediaWiki:About
      EOF
      trap cleanup EXIT
      cp ${wikiDump} "$tmpdir"
      chown mediawiki:nginx "$tmpdir/wikidump.xml.gz"
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

  sops.secrets.nixos-wiki-github-client-secret.owner = config.services.phpfpm.pools.mediawiki.user;

  services.mediawiki = {
    enable = true;
    webserver = "nginx";
    database.type = "postgres";
    nginx.hostName = hostname;
    uploadsDir = "/var/lib/mediawiki-uploads/";
    passwordFile = config.sops.secrets."nixos-wiki".path;

    extensions.SyntaxHighlight_GeSHi = null; # provides <SyntaxHighlight> tags
    extensions.ParserFunctions = null;
    extensions.Cite = null;
    extensions.VisualEditor = null;
    extensions.AuthManagerOAuth = pkgs.fetchzip {
      url = "https://github.com/mohe2015/AuthManagerOAuth/releases/download/v0.2.0/AuthManagerOAuth.zip";
      sha256 = "sha256-SMfUR2okG3Wp8FZG99V01w2nBcbEAVvHRzkzFGHi0ZY=";
      # postgresql support
      postFetch = ''
        cd $out
        patch -p1 < ${./0001-add-postgres-support.patch}
      '';
    }; # Github login
    extensions.ConfirmEdit = null; # Combat SPAM with a simple Captcha
    extensions.StopForumSpam = pkgs.fetchzip {
      url = "https://extdist.wmflabs.org/dist/extensions/StopForumSpam-REL1_40-71b57ba.tar.gz";
      hash = "sha256-g8v4zr11c2e4bY0BNipJ48miyAF4WTNvlSMgb/NxPBA=";
    };

    extraConfig = ''
      #$wgDebugLogFile = "/var/log/mediawiki/debug.log";

      # allow local login
      $wgAuthManagerOAuthConfig = [
        'github' => [
          'clientId'                => 'Iv1.95ed182c83df1d22',
          'clientSecret'            => file_get_contents("${config.sops.secrets.nixos-wiki-github-client-secret.path}"),
          'urlAuthorize'            => 'https://github.com/login/oauth/authorize',
          'urlAccessToken'          => 'https://github.com/login/oauth/access_token',
          'urlResourceOwnerDetails' => 'https://api.github.com/user'
        ],
      ];

      # Enable account creation globally
      $wgGroupPermissions['*']['createaccount'] = true;
      $wgGroupPermissions['*']['autocreateaccount'] = true;

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
