{
  nix2container,
  knot-resolver,
  writeText,
  writeScript,
  runtimeShell,
  busybox,
  headscale,
  tcpdump,
}: let
  kresd = knot-resolver.override {extraFeatures = true;};
  configFile = writeText "headscale.yaml" (builtins.toJSON {
    db_type = "sqlite3";
    db_path = "/var/lib/headscale/db.sqlite";
    server_url = "http://headscale.thalheim.io";
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
    #tls_letsencrypt_challenge_type = "TLS-ALPN-01";
    #tls_letsencrypt_listen: ":http"
    #tls_letsencrypt_cache_dir = "/var/lib/headscale/cache";
    ip_prefixes = [
      "fd7a:115c:a1e0::/48"
      "100.64.0.0/10"
    ];
    ephemeral_node_inactivity_timeout = "600m";
    tls_letsencrypt_hostname = "headscale.thalheim.io";
    acme_email = "joerg@thalheim.io";
  });
in
  nix2container.buildImage {
    name = "mic92/headscale";
    config = {
      Entrypoint = [
        (writeScript "init" ''
          #!${busybox}/bin/sh
          set -x
          export PATH=${busybox}/bin
          mkdir -p /bin /tmp
          busybox --install -s /bin

          for bin in ${tcpdump}/bin/*; do
            ln -s "$bin" /bin/$(basename $bin)
          done
          mkdir -p /etc/headscale
          ln -s ${configFile} /etc/headscale/config.yaml

          exec ${headscale}/bin/headscale serve
        '')
      ];
      Volumes = {
        "/var/lib/headscale" = {};
      };
    };
    layers = [
      (nix2container.buildLayer {
        deps = [
          headscale
          busybox
          tcpdump
        ];
      })
    ];
  }
