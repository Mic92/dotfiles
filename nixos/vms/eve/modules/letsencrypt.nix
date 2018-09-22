{...}: {
  fileSystems."/etc/letsencrypt" = {
    device = "/lxc/web/rootfs/etc/letsencrypt";
    fsType = "none";
    options = ["bind" "nofail"];
  };
}
