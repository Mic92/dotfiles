{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  # DID of the operator account on pds.thalheim.io. It only exists once the
  # account has been created (`pdsadmin account create`), so the knot and tack
  # stay off until it is filled in.
  ownerDid = "did:plc:h2dscnnfszyr2kugjcmmxof7";

  # Accounts get <name>.thalheim.io handles, which atproto verifies by
  # fetching /.well-known/atproto-did from that host.
  handles = [ "joerg.thalheim.io" ];

  pdsPort = 3000;
  knotAddr = "127.0.0.1:5555";
  tackAddr = "127.0.0.1:8081";

  proxy = addr: {
    proxyPass = "http://${addr}";
    proxyWebsockets = true;
  };
in
{
  imports = [
    self.inputs.tangled.nixosModules.knot-rs
    self.inputs.tack.nixosModules.default
  ];

  clan.core.vars.generators.bluesky-pds = {
    files.env = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
      pkgs.unixtools.xxd
    ];
    script = ''
      {
        echo "PDS_JWT_SECRET=$(openssl rand -hex 16)"
        echo "PDS_ADMIN_PASSWORD=$(openssl rand -hex 16)"
        echo "PDS_PLC_ROTATION_KEY_K256_PRIVATE_KEY_HEX=$(
          openssl ecparam -name secp256k1 -genkey -noout -outform DER |
            tail -c +8 | head -c 32 | xxd -p -c 32
        )"
      } > "$out/env"
    '';
  };

  # Losing this key makes the knot's sealed signing key unreadable.
  clan.core.vars.generators.tangled-knot = {
    files.env = { };
    runtimeInputs = [ pkgs.openssl ];
    script = ''
      printf 'KNOT_MASTER_KEY=%s\n' "$(openssl rand -base64 32)" > "$out/env"
    '';
  };

  # Shared between tack and nixbot, which tack drives as its Buildkite.
  clan.core.vars.generators.tangled-nixbot = {
    files.api-token = { };
    files.webhook-secret = { };
    files.tack-env = { };
    runtimeInputs = [ pkgs.openssl ];
    script = ''
      openssl rand -hex 32 | tr -d '\n' > "$out/api-token"
      openssl rand -hex 32 | tr -d '\n' > "$out/webhook-secret"
      {
        echo "TACK_BUILDKITE_TOKEN=$(cat "$out/api-token")"
        echo "TACK_BUILDKITE_WEBHOOK_SECRET=$(cat "$out/webhook-secret")"
      } > "$out/tack-env"
    '';
  };

  services.bluesky-pds = {
    enable = true;
    pdsadmin.enable = true;
    environmentFiles = [ config.clan.core.vars.generators.bluesky-pds.files.env.path ];
    settings = {
      PDS_PORT = pdsPort;
      PDS_HOSTNAME = "pds.thalheim.io";
      PDS_SERVICE_HANDLE_DOMAINS = ".thalheim.io";
    };
  };

  services.tangled.knot-rs = lib.mkIf (ownerDid != null) {
    enable = true;
    # "knot" and /var/lib/knot belong to knot-dns on this host.
    user = "tangled-knot";
    stateDir = "/var/lib/tangled-knot";
    environmentFile = config.clan.core.vars.generators.tangled-knot.files.env.path;
    settings = {
      server = {
        hostname = "knot.thalheim.io";
        admins = [ ownerDid ];
        listen_addr = knotAddr;
      };
      atproto.plc_directory = "https://plc.directory";
      contact.email = "joerg@thalheim.io";
      xrpc = {
        trusted_proxy_header = "x-forwarded-for";
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
    };
  };

  services.tangled.tack = lib.mkIf (ownerDid != null) {
    enable = true;
    hostname = "tack.thalheim.io";
    listenAddr = tackAddr;
    inherit ownerDid;
    contactEmail = "joerg@thalheim.io";
    environmentFile = config.clan.core.vars.generators.tangled-nixbot.files.tack-env.path;
    buildkite = {
      org = "nixbot";
      webhookMode = "signature";
    };
  };
  systemd.services.tack.environment.TACK_BUILDKITE_API_URL = "https://nixbot.thalheim.io/buildkite";

  services.nginx.virtualHosts = {
    "pds.thalheim.io" = {
      forceSSL = true;
      useACMEHost = "thalheim.io";
      locations."/" = proxy "127.0.0.1:${toString pdsPort}";
      extraConfig = "client_max_body_size 300m;";
    };
  }
  // lib.genAttrs handles (_: {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations."= /.well-known/atproto-did" = proxy "127.0.0.1:${toString pdsPort}";
  })
  // lib.optionalAttrs (ownerDid != null) {
    "knot.thalheim.io" = {
      forceSSL = true;
      useACMEHost = "thalheim.io";
      locations."/" = proxy knotAddr;
      # Git pushes and LFS uploads go through here.
      extraConfig = "client_max_body_size 0;";
    };
    # No redirects: knot2 and the Tangled web app refuse to follow them.
    "tack.thalheim.io" = {
      onlySSL = true;
      useACMEHost = "thalheim.io";
      locations."/" = proxy tackAddr;
    };
  };
}
