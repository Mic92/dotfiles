{
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
        "secrets file" = "/run/keys/rsyncd.secrets";
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 873 ];

  services.netdata.portcheck.checks.rsync.port = 873;

  users.users.rsyncd = {
    isSystemUser = true;
    group = "rsyncd";
    extraGroups = [ "keys" ];
  };

  deployment.keys."rsyncd.secrets" = {
    keyFile = ../secrets/rsyncd.secrets;
    user = "rsyncd";
  };

  users.groups.rsyncd = {};

  systemd.services.rsyncd.serviceConfig = {
    AmbientCapabilities = "cap_net_bind_service";
  };
}
