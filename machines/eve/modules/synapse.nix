{ config, pkgs, ... }:
let
  inherit (config.services.matrix-synapse.settings) server_name;
  nginx-vhost = "matrix.thalheim.io";
  element-web-thalheim.io =
    pkgs.runCommand "element-web-with-config" { nativeBuildInputs = [ pkgs.buildPackages.jq ]; }
      ''
        cp -r ${pkgs.element-web} $out
        chmod -R u+w $out
        jq '."default_server_config"."m.homeserver" = { "base_url": "https://${nginx-vhost}:443", "server_name": "${server_name}" }' \
          > $out/config.json < ${pkgs.element-web}/config.json
        ln -s $out/config.json $out/config.${nginx-vhost}.json
      '';
in
{
  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = "thalheim.io";
      public_baseurl = "https://${nginx-vhost}";
      enable_registration = false;
      dynamic_thumbnails = true;
      user_directory.search_all_users = true;
      # MatrixRTC / Element Call
      experimental_features = {
        # Room summary API, needed by Element Call to check call membership
        msc3266_enabled = true;
        # Delayed events, used for reliable call member state cleanup
        msc4140_enabled = true;
      };
      max_event_delay_duration = "24h";
      # Element Call refreshes call member events frequently
      rc_message = {
        per_second = 0.5;
        burst_count = 30;
      };
      rc_delayed_event_mgmt = {
        per_second = 1;
        burst_count = 20;
      };
      max_upload_size = "30M";
      listeners = [
        {
          port = 8043;
          bind_addresses = [ "127.0.0.1" ];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = [
                "client"
                "federation"
              ];
              compress = false;
            }
          ];
        }
      ];
      trusted_key_servers = [ { server_name = "matrix.org"; } ];

      oidc_providers = [
        {
          idp_id = "authelia";
          idp_name = "Authelia";
          issuer = "https://auth.thalheim.io";
          client_id = "synapse";
          client_secret_path = config.clan.core.vars.generators.synapse-oidc.files.client-secret.path;
          scopes = [
            "openid"
            "profile"
            "email"
          ];
          allow_existing_users = true;
          user_profile_method = "userinfo_endpoint";
          user_mapping_provider.config = {
            localpart_template = "{{ user.email.split('@')[0] }}";
            display_name_template = "{{ user.name }}";
            email_template = "{{ user.email }}";
          };
        }
      ];
    };
  };

  # OIDC client for Authelia: Synapse gets the plaintext secret,
  # Authelia gets the pbkdf2 digest (see authelia.nix).
  clan.core.vars.generators.synapse-oidc = {
    files.client-secret.owner = "matrix-synapse";
    files.client-secret-hash.secret = false;
    runtimeInputs = with pkgs; [
      coreutils
      openssl
      authelia
      gnused
    ];
    script = ''
      openssl rand -hex 32 | tr -d '\n' > "$out/client-secret"
      authelia crypto hash generate pbkdf2 --variant sha512 \
        --password "$(cat "$out/client-secret")" |
        sed 's/^Digest: //' > "$out/client-secret-hash"
    '';
  };

  # MatrixRTC backend for Element Call
  clan.core.vars.generators.matrix-livekit = {
    files."livekit-keys" = { };
    runtimeInputs = [
      pkgs.livekit
      pkgs.gawk
    ];
    script = ''
      livekit-server generate-keys | awk '/API Secret/{print "lk-jwt-service: " $3}' > $out/livekit-keys
    '';
  };

  services.livekit = {
    enable = true;
    keyFile = config.clan.core.vars.generators.matrix-livekit.files."livekit-keys".path;
    openFirewall = true;
    settings.rtc.use_external_ip = true;
  };

  services.lk-jwt-service = {
    enable = true;
    livekitUrl = "wss://${nginx-vhost}/livekit/sfu";
    keyFile = config.clan.core.vars.generators.matrix-livekit.files."livekit-keys".path;
    port = 8090;
  };

  systemd.services.matrix-synapse.after = [ "postgresql.service" ];
  services.postgresql.ensureUsers = [ { name = "matrix-synapse"; } ];

  services.nginx.virtualHosts.${nginx-vhost} = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_read_timeout 600;
      client_max_body_size 30M;
    '';
    locations."/_matrix".proxyPass = "http://127.0.0.1:8043";
    locations."/_synapse/client".proxyPass = "http://127.0.0.1:8043";

    locations."/livekit/jwt/".proxyPass = "http://127.0.0.1:8090/";
    locations."/livekit/sfu/" = {
      proxyPass = "http://127.0.0.1:7880/";
      proxyWebsockets = true;
    };

    locations."/".root = element-web-thalheim.io;
  };
}
