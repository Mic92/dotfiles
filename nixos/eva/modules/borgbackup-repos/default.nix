{ config, ... }: {
  fileSystems."/mnt/backup" = {
    device = "nasil1.in.tum.de:/srv/il1/share_il1/Project_DSE_NixOS_Backup";
    fsType = "nfs4";
    options = [
      "vers=3"
      "noatime"
      "nodiratime"
      "nofail"
    ];
  };

  systemd.services.borgbackup-repo-nfs-share.enable = false;
  systemd.services.borgbackup-repo-nfs-home.enable = false;

  services.borgbackup.repos = {
    eve = {
      user = "il1dsenixosbk";
      group = "il1dsenixosbk";
      path = "/mnt/backup/eve";
      authorizedKeys = [
        (builtins.readFile ./eve-borgbackup.pub)
      ];
    };

    nfs-home = {
      user = "il1dsenixosbk";
      group = "il1dsenixosbk";
      path = "/mnt/backup/nfs-home";
      authorizedKeys = [
        (builtins.readFile ./nfs-home-borgbackup.pub)
      ];
    };

    nfs-share = {
      user = "il1dsenixosbk";
      group = "il1dsenixosbk";
      path = "/mnt/backup/nfs-share";
      authorizedKeys = [
        (builtins.readFile ./nfs-share-borgbackup.pub)
      ];
    };

    turingmachine = {
      user = "il1dsenixosbk";
      group = "il1dsenixosbk";
      path = "/mnt/backup/turingmachine";
      authorizedKeys = [
        (builtins.readFile ./turingmachine-borgbackup.pub)
      ];
    };
  };

  systemd.services = {
    borgbackup-repo-eve.serviceConfig.User = "il1dsenixosbk";
    borgbackup-repo-nfs.serviceConfig.User = "il1dsenixosbk";
    borgbackup-repo-turingmachine.serviceConfig.User = "il1dsenixosbk";
  };

  # the backup storage only supports a single uid.
  users.users.il1dsenixosbk = {
    isSystemUser = true;
    uid = 27741;
    group = "il1dsenixosbk";
    openssh.authorizedKeys.keys = config.users.users.joerg.openssh.authorizedKeys.keys;
  };

  users.groups.il1dsenixosbk.gid = 27741;
}
