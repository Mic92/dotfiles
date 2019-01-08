{ config, pkgs, ... } : {
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
      host = "/tmp";
    };

    extraConfig = ''
      define('LDAP_AUTH_SERVER_URI', 'ldap://127.0.0.1:389/');
      define('LDAP_AUTH_USETLS', FALSE); // Enable TLS Support for ldaps://
      define('LDAP_AUTH_ALLOW_UNTRUSTED_CERT', TRUE); // Allows untrusted certificate
      define('LDAP_AUTH_BINDDN', 'cn=ttrss,ou=system,ou=users,dc=eve');
      define('LDAP_AUTH_BINDPW', file_get_contents('/run/keys/ttrss-ldap-password'));
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
    virtualHosts."rss.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "rss.devkid.net";
    };

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

  deployment.keys."ttrss-ldap-password" = {
    keyFile = ../secrets/ttrss-ldap-password;
    user = "tt_rss";
  };

  users.users.tt_rss.extraGroups = [ "keys" ];
  systemd.services.phpfpm-tt-rss.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.netdata.httpcheck.checks.ttrss = {
    url = "https://rss.devkid.net";
    regex = "Tiny Tiny RSS";
  };
}
