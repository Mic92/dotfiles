{
  dockerTools,
  writeText,
  writeScript,
  runtimeShell,
  busybox,
  headscale,
  tcpdump,
  cacert
}: let
  configFile = writeText "headscale.yaml" (builtins.toJSON {
    db_type = "sqlite3";
    db_path = "/var/lib/headscale/db.sqlite";
    #server_url = "https://headscale.thalheim.io";
    server_url = "https://headscale.thalheim.io";
    listen_addr = "0.0.0.0:8080";
    grpc_listen_addr = "0.0.0.0:50443";
    tls_client_auth_mode = "relaxed";
    private_key_path = "/var/lib/headscale/private.key";

    derp = {
      server = {
        enabled = true;
        region_id = 999;
        region_code = "headscale";
        region_name = "Headscale Embedded DERP";
        # udp
        stun_listen_addr = "0.0.0.0:3478";
      };
      urls = [
        "https://controlplane.tailscale.com/derpmap/default"
      ];
      auto_update_enabled = "true";
      update_frequency = "24h";
      disable_check_updates = false;
      logtail.enabled = false;
    };
    tls_letsencrypt_challenge_type = "HTTP-01";
    tls_letsencrypt_listen = ":8081";
    tls_letsencrypt_cache_dir = "/var/lib/headscale/cache";
    ip_prefixes = [
      "100.64.0.0/10"
      "fd7a:115c:a1e0::/48"
    ];
    ephemeral_node_inactivity_timeout = "600m";
    tls_letsencrypt_hostname = "headscale.thalheim.io";
    acme_email = "joerg@thalheim.io";
  });
in
  dockerTools.streamLayeredImage {
    name = "registry.fly.io/headscale-mic92";
    tag = "latest";
    config = {
      Env = [
        "SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt"
      ];
      Entrypoint = [
        (writeScript "init" ''
          #!${busybox}/bin/sh
          set -x
          export PATH=${busybox}/bin
          rm -rf /bin
          mkdir -p /bin /tmp
          busybox --install -s /bin

          for bin in ${tcpdump}/bin/*; do
            ln -sf "$bin" /bin/$(basename $bin)
          done
          mkdir -p /etc/headscale /var/run/
          ln -s ${configFile} /etc/headscale/config.yaml
          exec ${headscale}/bin/headscale serve
        '')
      ];
      Volumes."/var/lib/headscale" = {};
    };
    contents = [
      headscale
      busybox
      tcpdump
    ];
  }
