{ config, pkgs, ... }:

let
  database = {
    connection_string = "postgres:///dendrite?host=/run/postgresql";
    max_open_conns = 20;
  };
in
{
  # $ nix-shell -p dendrite --run 'generate-keys --private-key /tmp/key'
  sops.secrets.matrix-server-key = {};

  services.dendrite = {
    enable = true;
    httpPort = 8043;
    settings = {
      global = {
        server_name = "thalheim.io";
        # `private_key` has the type `path`
        # prefix a `/` to make `path` happy
        private_key = "/$CREDENTIALS_DIRECTORY/matrix-server-key";
        trusted_third_party_id_servers = [
          "matrix.org"
          "vector.im"
        ];
      };
      logging = [{
        type = "std";
        level = "warn";
      }];
      app_service_api = {
        inherit database;
        config_files = [ ];
      };
      client_api = {
        registration_disabled = true;
        rate_limiting.enabled = false;
      };
      media_api = {
        inherit database;
        max_file_size_bytes = 100 * 1024 * 1024;
        dynamic_thumbnails = true;
      };
      room_server = {
        inherit database;
      };
      push_server = {
        inherit database;
      };
      mscs = {
        inherit database;
        mscs = [ "msc2836" "msc2946" ];
      };
      sync_api = {
        inherit database;
        real_ip_header = "X-Real-IP";
      };
      key_server = {
        inherit database;
      };
      federation_api = {
        inherit database;
        key_perspectives = [
          {
            server_name = "matrix.org";
            keys = [
              {
                key_id = "ed25519:auto";
                public_key = "Noi6WqcDj0QmPxCNQqgezwTlBKrfqehY1u2FyWP9uYw";
              }
              {
                key_id = "ed25519:a_RXGa";
                public_key = "l8Hft5qXKn1vfHrg3p4+W8gELQVo8N13JkluMfmn2sQ";
              }
            ];
          }
        ];
        prefer_direct_fetch = false;
      };
      user_api = {
        account_database = database;
        device_database = database;
      };
    };
  };

  systemd.services.dendrite.serviceConfig.LoadCredential = [
    "matrix-server-key:${config.sops.secrets.matrix-server-key.path}"
  ];

  systemd.services.dendrite.after = [ "postgresql.service" ];
  services.postgresql = {
    ensureDatabases = [ "dendrite" ];
    ensureUsers = [
      {
        name = "dendrite";
        ensurePermissions."DATABASE dendrite" = "ALL PRIVILEGES";
      }
    ];
  };

  services.nginx.virtualHosts."matrix.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    serverAliases = [ "matrix.thalheim.io" ];
    locations."/_matrix".proxyPass = "http://127.0.0.1:${toString config.services.dendrite.httpPort}";
    locations."/".root = pkgs.element-web;
  };
}
