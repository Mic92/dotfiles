{ pkgs, config, ... }:
let
  mediawiki = pkgs.callPackage ../pkgs/mediawiki.nix {};
in {
  services.phpfpm.pools.mediawiki = {
    user = "mediawiki";
    group = "mediawiki";
    settings = {
      "listen.owner" = "nginx";
      "listen.group" = "nginx";
      "pm" = "dynamic";
      "pm.max_children" = 32;
      "pm.start_servers" = 1;
      "pm.min_spare_servers" = 1;
      "pm.max_spare_servers" = 1;
      "pm.max_requests" = 500;
    };
  };

	environment.etc."mediawiki/LocalSettings.php".text = ''
    <?php
    error_reporting( E_ALL );
    ini_set( 'display_errors', 1 );

    # Protect against web entry
    if ( !defined( 'MEDIAWIKI' ) ) {
        exit;
    }

    $wgReadOnly = "This wiki was discontinued. Editing is no longer possible";

    $wgSitename = "Informationssystemtechnik an der TU Dresden";
    $wgMetaNamespace = "IST";
    ## The URL base path to the directory containing the wiki;
    ## defaults for all runtime URL paths are based off of this.
    ## For more information on customizing the URLs
    ## (like /w/index.php/Page_title to /wiki/Page_title) please see:
    ## http://www.mediawiki.org/wiki/Manual:Short_URL
    $wgScriptPath = "";
    $wgScriptExtension = ".php";
    ## The protocol and server name to use in fully-qualified URLs
    $wgServer = "//ist.devkid.net";
    ## The relative URL path to the skins directory
    $wgStylePath = "$wgScriptPath/skins";
    ## The relative URL path to the logo.  Make sure you change this from the default,
    ## or else you'll overwrite your logo when you upgrade!
    $wgLogo             = "/logo.png";
    ## UPO means: this is also a user preference option
    $wgEnableEmail = true;
    $wgEnableUserEmail = true; # UPO

    $wgEmergencyContact = "devkid@devkid.net";
    $wgPasswordSender = "devkid@devkid.net";
    $wgEnotifUserTalk = true; # UPO
    $wgEnotifWatchlist = true; # UPO
    $wgEmailAuthentication = true;

    $wgDBtype = "postgres";
    $wgDBserver = "/run/postgresql";
    $wgDBname = "mediawiki";
    $wgDBuser = "mediawiki";

    $wgDBprefix = "";
    $wgShellLocale = "en_US.UTF-8";

    $wgLanguageCode = "de";

    $wgEnableUploads = true;
    $wgUploadDirectory = "/var/lib/mediawiki/uploads";

    wfLoadSkin( 'Vector' );
    $wgDefaultSkin = "vector";

    $wgArticlePath = "/wiki/$1";
    $wgUsePathInfo = true;
    $wgFavicon = "$wgScriptPath/favicon.png";

    $wgShowExceptionDetails = true;
    $wgShowSQLErrors = 1;
    $wgShowDBErrorBacktrace = true;

    # disable account creation
    $wgGroupPermissions['*']['createaccount'] = false;
    # disable editing
    $wgGroupPermissions['*']['edit'] = false;
    $wgGroupPermissions['user']['edit'] = false;
    ?>
  '';

  users.users.mediawiki = {
    isSystemUser = true;
    createHome = true;
    group = "mediawiki";
    home = "/var/lib/mediawiki";
  };

  users.groups.mediawiki = {};

  services.nginx = {
    virtualHosts."ist.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      locations."= /robots.txt".extraConfig = ''
        access_log off; log_not_found off;
      '';
      locations."= /facicon.ico".extraConfig = ''
        access_log off; log_not_found off;
      '';
      locations."/".extraConfig = ''
        try_files $uri $uri/ @rewrite;
      '';
      locations."@rewrite".extraConfig = ''
        rewrite ^/wiki([^?]*)(?:\?(.*))? /index.php?title=$1&$2 last;
      '';
      locations."~ \.(php|php5)$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param   SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_index   index.php;
        fastcgi_pass    unix:${config.services.phpfpm.pools.mediawiki.socket};
      '';
      extraConfig = ''
        add_header X-Frame-Options DENY;

        index index.php index.html index.htm;

      # anti spam
        rewrite ^/richtige-anmeldung.php$ /index.php?title=Spezial:Anmelden&type=signup&spam=nospam;
        if ($arg_title = Spezial:Anmelden) { set $rewritecond "1"; }
        if ($arg_type = signup) { set $rewritecond "''${rewritecond}2"; }
        if ($arg_spam != nospam) { set $rewritecond "''${rewritecond}3"; }
        if ($arg_action != submitlogin) { set $rewritecond "''${rewritecond}4"; }
        if ($rewritecond = 1234) { rewrite ^ /anmeldung.php last; }
      '';
      root = "${mediawiki}/share/mediawiki";
    };
    virtualHosts."www.ist.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      globalRedirect = "ist.devkid.net";
    };
  };

  services.netdata.httpcheck.checks.istwiki = {
    url = "https://ist.devkid.net";
    regex = "Informationssystemtechnik";
  };
}
