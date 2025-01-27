{
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [
    inputs.data-mesher.nixosModules.data-mesher
  ];

  services.data-mesher = {
    enable = true;
    openFirewall = true;

    settings = {
      log_level = "debug";

      key_path = config.clan.core.vars.generators.data-mesher.files."dm.pem".path;
      host.names = [
        "turingmachine"
        #"earth"
      ];
      network.public_key = "MCowBQYDK2VwAyEAL+ajPxkanYcLFURnlyo31uFEzuymbDyvtMcYjT/H9nw=";
      cluster = {
        interface = "tailscale0";
        port = 7946;
        bootstrap_peers = [
          "[fd7a:115c:a1e0::8f01:9e1f]:7946"
        ];
        push_pull_interval = "15s";
      };
    };

    #initNetwork = {
    #  enable = true;
    #  tld = "sol";
    #  hostTTL = "1m";
    #  signingKeys = [
    #    "MCowBQYDK2VwAyEAL+ajPxkanYcLFURnlyo31uFEzuymbDyvtMcYjT/H9nw="
    #  ];
    #};
  };
  services.tailscale.enable = true;

  clan.core.vars.generators.data-mesher = {
    files."dm.pem".owner = "data-mesher";
    runtimeInputs = [
      pkgs.openssl
    ];
    script = ''
      openssl genpkey -algorithm ed25519 -out $out/dm.pem
    '';
  };
}
