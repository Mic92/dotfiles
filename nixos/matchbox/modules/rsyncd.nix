{ config, ... }: {
  services.rsyncd = {
    enable = true;
    settings = {
      global = {
        user = "rsyncd";
        group = "rsyncd";
        "use chroot" = false;
      };
      public = {
        comment = "Public rsync share.";
        path = "/mnt/hdd/public";
        "auth users" = "backup";
        "secrets file" = config.sops.secrets.rsyncd-secrets.path;
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 873 ];

  users.users.rsyncd = {
    isSystemUser = true;
    group = "rsyncd";
  };

  sops.secrets.rsyncd-secrets.owner = "rsyncd";

  users.groups.rsyncd = { };

  systemd.services.rsyncd.serviceConfig = {
    AmbientCapabilities = "cap_net_bind_service";
  };
}
