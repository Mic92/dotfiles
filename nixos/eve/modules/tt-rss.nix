{ config
, pkgs
, ...
}: {
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
      define('LDAP_AUTH_BINDPW', file_get_contents('${config.sops.secrets.ttrss-ldap-password.path}'));
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

  # NOTE: No configuration is done if not using virtual host
  services.nginx = {
    virtualHosts."rss.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = config.services.tt-rss.virtualHost;
    };

    virtualHosts.${config.services.tt-rss.virtualHost} = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
    };
  };

  sops.secrets.ttrss-ldap-password.owner = "tt_rss";
}
