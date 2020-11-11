{ config, pkgs, lib, ... }: let
  rulerConfig = {};

  rulerDir = pkgs.writeText "ruler.yml" (builtins.toJSON rulerConfig);

  ldapConf = pkgs.writeText "ldap.conf" ''
    base dc=eve
    host localhost:389
    pam_login_attribute mail
    pam_filter objectClass=loki
    binddn cn=nginx,ou=system,ou=users,dc=eve
  '';
in {
  services.loki = {
    enable = true;
    configuration = {
      # Basic stuff
      auth_enabled = false;
      server = {
        http_listen_port = 3100;
        log_level = "warn";
      };

      # Distributor
      distributor.ring.kvstore.store = "inmemory";

      # Ingester
      ingester = {
        lifecycler.ring = {
          kvstore.store = "inmemory";
          replication_factor = 1;
        };
        chunk_encoding = "snappy";
        # Disable block transfers on shutdown
        max_transfer_retries = 0;
      };

      # Storage
      storage_config = {
        boltdb.directory = "/var/lib/loki/boltdb";
        filesystem.directory = "/var/lib/loki/storage";
      };

      # Table manager
      table_manager = {
        retention_deletes_enabled = true;
        retention_period = "120h";
      };

      # Schema
      schema_config.configs = [{
        from = "2020-11-08";
        store = "boltdb";
        object_store = "filesystem";
        schema = "v11";
        index.prefix = "index_";
      }];

      limits_config.ingestion_burst_size_mb = 16;

      #ruler = {
      #  storage = {
      #    type = "local";
      #    directory = rulerDir;
      #  };
      #  rule_path = "/var/lib/loki/ruler";
      #  alertmanager_url = "https://alerts.helsinki.tools";
      #  ring.kvstore = "inmemory";
      #};

      # Query splitting and caching
      query_range = {
        split_queries_by_interval = "24h";
        cache_results = true;
      };
    };
  };

  systemd.services.ldap-nginx-config = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      umask 0077
      printf 'bindpw %s\n' \
        "$(cat ${config.sops.secrets.ldap-nginx-password.path})" | \
      cat "${ldapConf}" - > /run/ldap-nginx.conf
      chown nginx /run/ldap-nginx.conf
    '';
    serviceConfig.Type = "oneshot";
  };
  sops.secrets.ldap-nginx-password = {};

  security.pam.services.loki.text = ''
    auth required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=/run/ldap-nginx.conf
    account required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=/run/ldap-nginx.conf
  '';

  services.nginx = {
    enable = true;
    package = pkgs.nginxStable.override {
      perl = null;
      modules = [
        pkgs.nginxModules.pam
      ];
    };
    virtualHosts.loki = {
      serverName = "loki.r";
      locations."/" = {
        proxyWebsockets = true;
        extraConfig = ''
          auth_pam "Ldap password";
          auth_pam_service_name "loki";

          proxy_read_timeout 1800s;
          proxy_redirect off;
          proxy_connect_timeout 1600s;

          access_log off;
          proxy_pass http://127.0.0.1:3100;
        '';
      };
      locations."/ready" = {
        proxyWebsockets = true;
        extraConfig = ''
          auth_basic off;
          access_log off;
          proxy_pass http://127.0.0.1:3100;
        '';
      };
    };
  };

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 80 ];
}
