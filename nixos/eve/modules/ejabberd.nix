{ pkgs
, config
, ...
}: {
  services.ejabberd = {
    enable = true;
    configFile = "/etc/ejabberd.yml";
    package = pkgs.ejabberd.override {
      withPgsql = true;
      withTools = true;
    };
  };

  services.postgresql.ensureDatabases = [ "ejabberd" ];
  services.postgresql.ensureUsers = [
    {
      name = "ejabberd";
      ensureDBOwnership = true;
    }
  ];

  security.dhparams = {
    enable = true;
    params.nginx = { };
  };

  environment.etc."ejabberd.yml" = {
    user = "ejabberd";
    mode = "0600";
    text = ''
      loglevel: 4

      auth_method: ldap
      ldap_servers:
        - localhost
      ldap_base: "ou=users,dc=eve"
      ldap_uids:
        jabberID: "%u@%d"
      ldap_rootdn: "cn=ejabberd,ou=system,ou=users,dc=eve"
      ldap_filter: "(objectClass=jabberUser)"
      # ldap_password: ""
      include_config_file:
        - ${config.sops.secrets."ejabber-ldap-password.yml".path}

      default_db: sql
      new_sql_schema: true
      sql_type: pgsql
      sql_server: 127.0.0.1
      sql_port: 5432
      sql_username: ejabberd
      sql_database: ejabberd
      # sql_password: ejabberd
      include_config_file:
        - ${config.sops.secrets."ejabber-postgres-password.yml".path}

      hosts:
      - thalheim.io
      - anon.thalheim.io
      - devkid.net
      - w01f.de

      s2s_cafile: "/etc/ssl/certs/ca-certificates.crt"

      certfiles:
      - /var/lib/acme/ejabberd-anon.thalheim.io/full.pem
      - /var/lib/acme/ejabberd-thalheim.io/full.pem
      - /var/lib/acme/ejabberd-devkid.net/full.pem
      listen:
      -
        port: 5222
        ip: "::"
        module: ejabberd_c2s
        max_stanza_size: 262144
        shaper: c2s_shaper
        access: c2s
        starttls_required: true
        dhfile: "${config.security.dhparams.params.nginx.path}"
      -
        port: 5269
        ip: "::"
        module: ejabberd_s2s_in
        max_stanza_size: 524288
        dhfile: "${config.security.dhparams.params.nginx.path}"
      -
        port: 5443
        ip: "::"
        module: ejabberd_http
        tls: true
        request_handlers:
          /admin: ejabberd_web_admin
          /api: mod_http_api
          /bosh: mod_bosh
          /captcha: ejabberd_captcha
          /upload: mod_http_upload
          /ws: ejabberd_http_ws
        dhfile: "${config.security.dhparams.params.nginx.path}"
      -
        port: 5280
        ip: "::"
        module: ejabberd_http
        request_handlers:
          /admin: ejabberd_web_admin
          /.well-known/acme-challenge: ejabberd_acme
      -
        port: 1883
        ip: "::"
        module: mod_mqtt
        backlog: 1000

      s2s_use_starttls: required

      acl:
        local:
          user_regexp: ""
        loopback:
          ip:
            - 127.0.0.0/8
            - ::1/128
        hass_publisher:
          user:
            "bme680" : "thalheim.io"
            "hass" : "thalheim.io"
        hass_subscriber:
          user:
            "hass" : "thalheim.io"
      access_rules:
        local:
          allow: local
        c2s:
          deny: blocked
          allow: all
        s2s:
          - allow
        announce:
          allow: admin
        configure:
          allow: admin
        muc_create:
          allow: all
        pubsub_createnode:
          allow: local
        trusted_network:
          allow: loopback
      api_permissions:
        "console commands":
          from:
            - ejabberd_ctl
          who: all
          what: "*"
        "admin access":
          who:
            access:
              allow:
                acl: loopback
                acl: admin
            oauth:
              scope: "ejabberd:admin"
              access:
                allow:
                  acl: loopback
                  acl: admin
          what:
            - "*"
            - "!stop"
            - "!start"
        "public commands":
          who:
            ip: 127.0.0.1/8
          what:
            - status
            - connected_users_number
      shaper:
        normal: 1000
        fast: 50000

      shaper_rules:
        max_user_sessions: 10
        max_user_offline_messages:
          5000: admin
          100: all
        c2s_shaper:
          none: admin
          normal: all
        s2s_shaper: fast
      modules:
        mod_adhoc: {}
        mod_admin_extra: {}
        mod_announce:
          access: announce
        mod_avatar: {}
        mod_blocking: {}
        mod_bosh: {}
        mod_caps: {}
        mod_carboncopy: {}
        mod_client_state: {}
        mod_configure: {}
        mod_disco: {}
        mod_fail2ban: {}
        mod_http_api: {}
        mod_http_upload:
          put_url: https://@HOST@:5443/upload
        mod_last: {}
        mod_mam:
          ## Mnesia is limited to 2GB, better to use an SQL backend
          ## For small servers SQLite is a good fit and is very easy
          ## to configure. Uncomment this when you have SQL configured:
          ## db_type: sql
          assume_mam_usage: true
          default: always
        mod_mqtt:
          access_publish:
            "homeassistant/#":
              - allow: hass_publisher
              - deny
            "#":
              - deny
          access_subscribe:
            "homeassistant/#":
              - allow: hass_subscriber
              - deny
            "#":
              - deny
        mod_muc:
          host: "muc.@HOST@"
          access:
            - allow
          access_admin:
            - allow: admin
          access_create: muc_create
          access_persistent: muc_create
          access_mam:
            - allow
          default_room_options:
            mam: true
        mod_muc_admin: {}
        mod_offline:
          access_max_user_messages: max_user_offline_messages
        mod_ping: {}
        mod_privacy: {}
        mod_private: {}
        mod_proxy65:
          access: local
          max_connections: 5
        mod_pubsub:
          access_createnode: pubsub_createnode
          plugins:
            - flat
            - pep
          force_node_config:
            ## Avoid buggy clients to make their bookmarks public
            storage:bookmarks:
              access_model: whitelist
        mod_push: {}
        mod_push_keepalive: {}
        mod_register:
          ## Only accept registration requests from the "trusted"
          ## network (see access_rules section above).
          ## Think twice before enabling registration from any
          ## address. See the Jabber SPAM Manifesto for details:
          ## https://github.com/ge0rg/jabber-spam-fighting-manifesto
          ip_access: trusted_network
        mod_roster:
          versioning: true
        mod_s2s_dialback: {}
        mod_shared_roster: {}
        mod_stream_mgmt:
          resend_on_timeout: if_offline
        mod_vcard: {}
        mod_vcard_xupdate: {}
        mod_version:
          show_os: false
    '';
  };

  sops.secrets."ejabber-ldap-password.yml".owner = "ejabberd";
  sops.secrets."ejabber-postgres-password.yml".owner = "ejabberd";

  security.acme.certs =
    let
      cert = domain: {
        inherit domain;
        postRun = "systemctl restart ejabberd.service";
        group = "ejabberd";
        dnsProvider = "rfc2136";
        credentialsFile = config.sops.secrets.lego-knot-credentials.path;
        extraDomainNames = [ "*.${domain}" ];
      };
    in
    {
      "ejabberd-anon.thalheim.io" = cert "anon.thalheim.io";
      "ejabberd-devkid.net" = cert "devkid.net";
      "ejabberd-thalheim.io" = cert "thalheim.io";
    };

  services.tor.relay.onionServices."jabber".map = [
    { port = 5222; }
    { port = 5269; }
  ];

  networking.firewall.allowedTCPPorts = [
    5222 # xmpp-client
    5269 # xmpp-server
    5280 # xmpp-bosh
    5443 # https
    1883 # mqtt
    # which port for proxy64?
    #6555 # xmpp-proxy65
  ];
}
