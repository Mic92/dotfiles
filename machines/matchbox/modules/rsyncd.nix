{ config, pkgs, ... }:
{
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
        "secrets file" = config.clan.core.vars.generators.rsyncd.files.secret-file.path;
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 873 ];

  users.users.rsyncd = {
    isSystemUser = true;
    group = "rsyncd";
  };

  users.groups.rsyncd = { };

  systemd.services.rsyncd.serviceConfig = {
    AmbientCapabilities = "cap_net_bind_service";
  };

  clan.core.vars.generators.rsyncd = {
    files.secret-file.owner = "rsyncd";
    runtimeInputs = with pkgs; [ openssl ];
    script = ''
      echo "backup:$(openssl rand -base64 32)" > "$out"/secret-file
    '';
    share = true;
  };
}
