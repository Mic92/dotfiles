{
  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    extraOptions = "--iptables=false -D";
  };

  users.users.netdata.extraGroups = [ "docker" ];
}
