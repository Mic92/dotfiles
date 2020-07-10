{ config, pkgs, ... } : {

  services.phpfpm.pools.tt-rss.settings = {
    "pm" = "ondemand";
    "pm.max_children" = 32;
    "pm.process_idle_timeout" = "10s";
    "pm.max_requests" = 500;
  };

  services.tt-rss = {
    enable = true;
    virtualHost = "rss.devkid.net";
    selfUrlPath = "https://rss.devkid.net";

    pluginPackages = [
      pkgs.tt-rss-plugin-auth-ldap
    ];
    themePackages = [
      pkgs.tt-rss-theme-feedly
    ];
    plugins = [
      "auth_internal"
      "auth_ldap"
      "note"
      "updater"
      "api_feedreader"
    ];

    database = {
      type = "pgsql";
      password = "";
      host = "/run/postgresql";
    };

    extraConfig = ''
      define('LDAP_AUTH_SERVER_URI', 'ldap://127.0.0.1:389/');
      define('LDAP_AUTH_USETLS', FALSE); // Enable TLS Support for ldaps://
      define('LDAP_AUTH_ALLOW_UNTRUSTED_CERT', TRUE); // Allows untrusted certificate
      define('LDAP_AUTH_BINDDN', 'cn=ttrss,ou=system,ou=users,dc=eve');
      define('LDAP_AUTH_BINDPW', file_get_contents('${config.krops.secrets."ttrss-ldap-password".path}'));
      define('LDAP_AUTH_BASEDN', 'dc=eve');
      define('LDAP_AUTH_LOGIN_ATTRIB', 'mail');
      define('LDAP_AUTH_ANONYMOUSBEFOREBIND', FALSE);
      // ??? will be replaced with the entered username(escaped) at login
      define('LDAP_AUTH_SEARCHFILTER', '(&(objectClass=ttrss)(mail=???))');
      // Optional configuration
      define('LDAP_AUTH_LOG_ATTEMPTS', FALSE);
      // Enable Debug Logging
      define('LDAP_AUTH_DEBUG', FALSE);
    '';
  };

  services.nginx = {
    virtualHosts."rss.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "rss.devkid.net";
    };

    virtualHosts."rss.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
    };
  };

  krops.secrets.ttrss-ldap-password.owner = "tt_rss";

  users.users.tt_rss.extraGroups = [ "keys" ];
  systemd.services.phpfpm-tt-rss.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.netdata.httpcheck.checks.ttrss = {
    url = "https://rss.devkid.net";
    regex = "Tiny Tiny RSS";
  };

  services.openldap.extraConfig = ''
    objectClass ( 1.3.6.1.4.1.28294.1.2.4 NAME 'ttrss'
            SUP top AUXILIARY
            DESC 'Added to an account to allow tinytinyrss access'
    	MUST ( mail $ userPassword ))
  '';

  services.icinga2.extraConfig = ''
    apply Service "TT-RSS v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "rss.devkid.net"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "TT-RSS v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "rss.devkid.net"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
