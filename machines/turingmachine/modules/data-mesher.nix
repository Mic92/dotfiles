{ inputs, ... }:
{
  imports = [
    inputs.data-mesher.nixosModules.data-mesher
  ];

  services.data-mesher = {
    enable = true;
    openFirewall = true;

    settings = {
      log_level = "debug";

      key_path = "/run/dm.pem";
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
}
