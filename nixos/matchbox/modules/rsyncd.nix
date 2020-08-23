{ config, ... }: {
  services.rsyncd ={
    enable = true;
    user = "rsyncd";
    group = "rsyncd";
    extraConfig = ''
      use chroot = false
    '';
    modules =  {
      public = {
        comment = "Public rsync share.";
        path = "/mnt/hdd/public";
        "auth users" = "backup";
        "secrets file" = config.krops.secrets."rsyncd.secrets".path;
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 873 ];

  users.users.rsyncd = {
    isSystemUser = true;
    group = "rsyncd";
    extraGroups = [ "keys" ];
  };

  krops.secrets."rsyncd.secrets".owner = "rsyncd";

  users.groups.rsyncd = {};

  systemd.services.rsyncd.serviceConfig = {
    AmbientCapabilities = "cap_net_bind_service";
  };
}
