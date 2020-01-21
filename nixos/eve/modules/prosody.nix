{ pkgs, ... }: {
  services.prosody = {
    enable = true;
    admins = [
      "joerg@thalheim.io"
      "devkid@devkid.net"
    ];
    extraConfig = ''
      consider_bosh_secure = true
      cross_domain_bosh = {
        "thalheim.io",
        "devkid.net",
        "higgsboson.tk",
        "muc.higgsboson.tk",
        "anon.higgsboson.tk"
      }
      default_archive_policy = true
      proxy65_ports = {
        6555
      }
      allow_registration = false

      authentication = "ldap"
      ldap_server = "127.0.0.1"
      ldap_tls = false
      ldap_base = "ou=users,dc=eve"
      ldap_scope = "subtree"
      ldap_filter = "(&(jabberID=$user@$host)(objectClass=jabberUser))"
      ldap_rootdn = "cn=prosody,ou=system,ou=users,dc=eve"
      ldap_password = (io.open("/run/keys/prosody-ldap-password", "r"):read("*a"))

      default_storage = "sql"
      storage = {
        -- This makes mod_mam use the sql2 storage backend (others will use internal)
        -- which at the time of this writing is the only one supporting stanza archives
        archive2 = "sql";
      }
      http_upload_file_size_limit = 20 * 1024 * 1024

      sql = {
        driver = "PostgreSQL",
        database = "prosody",
        username = "prosody",
      }

      Component "muc.higgsboson.tk" "muc"
        modules_enabled = { "mam_muc"; }
      muc_log_by_default = true;
      muc_log_all_rooms = false;
      max_archive_query_results = 20;
      max_history_messages = 1000;
      -- Set up a SOCKS5 bytestream proxy for server-proxied file transfers:
      Component "jabber.higgsboson.tk" "proxy65"
      Component "proxy.higgsboson.tk" "proxy65"
      -- Feeds!
      Component "pubsub.higgsboson.tk" "pubsub"
      Component "jabber.higgsboson.tk" "http_upload"

      ssl = {
        extraOptions = {
          dhparam = "/var/lib/prosody/dh-2048.pem";
          ciphers = "HIGH+kEDH:HIGH+kEECDH:!DHE-RSA-AES128-GCM-SHA256:!DHE-RSA-AES128-SHA256:!ECDHE-RSA-AES128-GCM-SHA256:!ECDHE-RSA-AES128-SHA256:!ECDHE-RSA-AES128-SHA:!AES128-GCM-SHA256:!AES256-GCM-SHA384:!AES256-SHA256:AES128-SHA256:!CAMELLIA256-SHA:AES256-SHA:!DHE-RSA-CAMELLIA128-SHA:!DHE-DSS-CAMELLIA128-SHA:!DHE-RSA-AES128-SHA:!DHE-DSS-AES128-SHA:HIGH:!CAMELLIA128-SHA:!AES128-SHA:!SRP:!3DES:!aNULL";
        };
      };
    '';
    s2sSecureDomains = [
      # wh00t?
      #"jabber.c3d2.de"
    ];
    modules = {
      mam = true;
      bosh = true;
      http_files = true;
      watchregistrations = true;
      proxy65 = true;
    };
    virtualHosts = {
      thalheim = {
        domain = "thalheim.io";
        enabled = true;
        ssl.key  = "/var/lib/acme/prosody-thalheim.io/key.pem";
        ssl.cert = "/var/lib/acme/prosody-thalheim.io/fullchain.pem";
      };
      higgsboson = {
        domain = "higgsboson.tk";
        enabled = true;
        ssl.key  = "/var/lib/acme/prosody-higgsboson.tk/key.pem";
        ssl.cert = "/var/lib/acme/prosody-higgsboson.tk/fullchain.pem";
      };
      devkid = {
        domain = "devkid.net";
        enabled = true;
        ssl.key  = "/var/lib/acme/prosody-devkid.net/key.pem";
        ssl.cert = "/var/lib/acme/prosody-devkid.net/fullchain.pem";
      };
      w01f = {
        domain = "w01f.de";
        enabled = true;
      };
      anon = {
        enabled = true;
        domain = "anon.higgsboson.tk";
        ssl.key  = "/var/lib/acme/prosody-anon.higgsboson.tk/key.pem";
        ssl.cert = "/var/lib/acme/prosody-anon.higgsboson.tk/fullchain.pem";
        extraConfig = ''
          authentication = "anonymous"
        '';
      };
    };
    package = pkgs.prosody.override {
      withExtraLibs = [
        pkgs.nur.repos.mic92.lualdap
        pkgs.luaPackages.luadbi-postgresql
      ];
      withCommunityModules = [
        "smacks"
        "smacks_offline"
        "csi"
        # crititcal bug -> TODO report upstream
        # mod_s2s: Traceback[s2s]: ...ib/prosody/modules/mod_cloud_notify/mod_cloud_notify.lua:255:  attempt to perform arithmetic on field 'count' (a nil value)
        #"cloud_notify"
        "throttle_presence"
        "http_upload"
        "pep_vcard_avatar"
        "auth_ldap"
      ];
    };
  };

  security.acme.certs = let
    cert = domain: {
      inherit domain;
      webroot = "/var/lib/acme/acme-challenge";
      postRun = "systemctl restart prosody.service";
      allowKeysForGroup = true;
      group = "prosody";
    };
  in {
    "prosody-anon.higgsboson.tk" = cert "anon.higgsboson.tk";
    "prosody-devkid.net" = cert "devkid.net";
    "prosody-thalheim.io" = cert "thalheim.io";
    "prosody-higgsboson.tk" = cert "higgsboson.tk";
  };
  services.nginx.virtualHosts."anon.higgsboson.tk".useACMEHost = "thalheim.io";

  users.users.prosody.extraGroups = [ "keys" ];
  systemd.services.prosody.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.tor.hiddenServices."jabber".map = [
    { port = "5222"; }
    { port = "5269"; }
  ];

  services.netdata.portcheck.checks = {
    xmpp-server.port = 5222;
    xmpp-client.port = 5269;
  };

  networking.firewall.allowedTCPPorts = [
    5222 # xmpp-client
    5269 # xmpp-server
    5280 # xmpp-bosh
    5281 # bosh-ssl
    6555 # xmpp-proxy65
  ];

  krops.secrets.files.prosody-ldap-password.owner = "prosody";

  services.openldap.extraConfig = ''
    attributeType ( 1.2.752.43.9.1.1
        NAME 'jabberID'
        DESC 'The Jabber ID(s) associated with this object. Used to map a JID to an LDAP account.'
        EQUALITY caseIgnoreMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 )

    objectClass ( 1.2.752.43.9.2.1
        NAME 'jabberUser'
        DESC 'A jabber user'
        AUXILIARY
        MUST ( jabberID ) )
  '';

  services.icinga2.extraConfig = ''
    apply Service "JABBER C2S (eve)" {
      import "eve-service"
      check_command = "xmpp_cert6"
      vars.ssl_cert_port = "5222"
      vars.ssl_cert_xmpphost = "thalheim.io";
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "JABBER S2S (eve)" {
      import "eve-service"
      check_command = "xmpp_cert4"
      vars.ssl_cert_port = "5269"
      vars.ssl_cert_xmpphost = "thalheim.io";
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "JABBER C2S v6 (eve)" {
      import "eve-service"
      check_command = "xmpp_cert6"
      vars.ssl_cert_port = "5222"
      vars.ssl_cert_xmpphost = "thalheim.io";
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "JABBER S2S v6 (eve)" {
      import "eve-service"
      check_command = "xmpp_cert6"
      vars.ssl_cert_port = "5269"
      vars.ssl_cert_xmpphost = "thalheim.io";
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
