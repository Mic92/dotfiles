{ pkgs, ... }: {
  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    extraOptions = "--dns 172.28.0.1";
    autoPrune.enable = true;
  };

  # see ./drone.nix
  services.kresd.listenPlain = [ "172.28.0.1:53" ];

  environment.etc."docker/daemon.json".text = builtins.toJSON {
    ipv6 = true;
    "fixed-cidr-v6" = "2a01:4f9:2b:1605:2::1/80";
  };
}
