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
  jq,
  pkgs,
}: let
  acls = writeText "acl.yaml" (builtins.toJSON {
    # A device tagged security can advertise exit nodes that are auto-approved
    autoApprovers = {
      exitNode = ["tag:exitnode"];
    };
    acls = [
      {
        action = "accept";
        src = ["tag:all"];
        dst = ["tag:all:*"];
      }
    ];
  });
  configFile = writeText "headscale.yaml" (builtins.toJSON {
    db_type = "sqlite3";
    db_path = "/var/lib/headscale/db.sqlite";
    #server_url = "https://headscale.thalheim.io";
    server_url = "https://krebscale.thalheim.io";
    listen_addr = "0.0.0.0:8080";
    grpc_listen_addr = "0.0.0.0:50443";
    tls_client_auth_mode = "relaxed";
    private_key_path = "/var/lib/headscale/private.key";

    #acl_policy_path = acls;
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
    tls_letsencrypt_hostname = "krebscale.thalheim.io";
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
    while [[ ! -S /var/run/tailscale/tailscaled.sock ]]; do
      sleep 1
    done

    while [[ ! -S /var/run/headscale.sock ]]; do
      sleep 1
    done

    if ! headscale namespaces list | grep -q krebscale; then
      headscale namespace create krebscale
    fi
    key=$(headscale --namespace krebscale preauthkeys create -o json --reusable --expiration 5m | jq -r .key)
    while ! tailscale up --advertise-exit-node --login-server https://headscale.thalheim.io  --authkey "$key"; do
      sleep 1
    done
    id=$(headscale nodes list -o json | jq -r '.[] | select(.name == "headscale") | .id')
    headscale routes enable --identifier "$id" --route '0.0.0.0/0' --route '::/0'
    killall tailscaled
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
    name = "registry.fly.io/krebscale";
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
          ln -s ${jq}/bin/jq /bin/jq
          ln -s ${iptables-legacy}/bin/iptables /bin/iptables
          ln -s ${iptables-legacy}/bin/ip6tables /bin/ip6tables

          hostname krebscale

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
