{
  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    extraOptions = "--dns 8.8.8.8";
  };

  users.users.netdata.extraGroups = [ "docker" ];
}
