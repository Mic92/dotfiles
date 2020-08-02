{
  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    extraOptions = "--dns 8.8.8.8";
  };

  users.users.netdata.extraGroups = [ "docker" ];

  environment.etc."docker/daemon.json".text = builtins.toJSON {
    ipv6 = true;
    "fixed-cidr-v6" = "2a01:4f9:2b:1605:2::1/80";
  };
}
