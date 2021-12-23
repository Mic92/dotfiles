{
  filesystems."/mnt/backup" = {
    device = "nasil1.in.tum.de:/srv/il1/share_il1/Project_DSE_NixOS_Backup";
    fsType = "nfs4";
    options = [
      "vers=3"
      "noatime"
      "nodiratime"
      "nofail"
    ];
  };
}
