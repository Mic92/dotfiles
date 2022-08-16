{
  dockerTools,
  writeText,
  writeScript,
  runtimeShell,
  busybox,
  headscale,
  tailscale,
  iptables-legacy,
  tcpdump,
  cacert,
  pkgs
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

  # FIXME: do I need iptables ?
  tailscale' = tailscale.overrideAttrs (old: {
    # avoid unecessary binaries
    postInstall = "";
  });

  setupHeadscale = pkgs.writeScript "setup-headscale" ''
    #!${busybox}/bin/sh
    set -eux -o pipefail
    while [[ ! -S /var/run/tailscale/tailscaled.sock ]]; then
      sleep 1
    done
    while ! headscale nodes list; do
      sleep 1
    done

    if ! headscale namespaces list | grep -q krebscale; then
      headscale namespace create krebscale
    fi
    key=$(headscale --namespace krebscale preauthkeys create --reusable --expiration 1h | grep -v "An updated version")
    while ! tailscale up --advertise-exit-node --login-server https://headscale.thalheim.io  --authkey "$key"; do
      sleep 1
    done
  '';

  headscaleSvc = pkgs.writeText "headscale" ''
    #!${busybox}/bin/sh
    exec ${headscale}/bin/headscale serve
  '';

  tailscaleSvc = pkgs.writeText "tailscale" ''
    #!${busybox}/bin/sh
    exec ${tailscale'}/bin/tailscaled --state=/var/lib/headscale/tailscaled.state --socket=/var/run/tailscale/tailscaled.sock --port 41641
  '';

  services = pkgs.runCommand "services" {} ''
    install -D -m755 ${headscaleSvc} $out/headscale/run
    install -D -m755 ${tailscaleSvc} $out/tailscale/run
  '';
  
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
          export PATH=/bin

          for bin in ${tcpdump}/bin/*; do
            ln -s "$bin" /bin/$(basename $bin)
          done
          ln -s ${headscale}/bin/headscale /bin/headscale
          ln -s ${tailscale'}/bin/tailscale /bin/tailscale
          ln -s ${iptables-legacy}/bin/iptables /bin/iptables
          ln -s ${iptables-legacy}/bin/ip6tables /bin/ip6tables

          hostname headscale

          modprobe xt_mark
          sysctl -w net.ipv4.ip_forward=1
          sysctl -w net.ipv6.conf.all.forwarding=1
          iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
          ip6tables -t nat -A POSTROUTING -o eth0 -j MASQUERADE

          mkdir -p /etc/headscale /var/run/
          ln -s ${configFile} /etc/headscale/config.yaml

          ${setupHeadscale} &
          exec ${busybox}/bin/runsvdir ${services}
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
