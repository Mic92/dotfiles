{ pkgs
, ...
}: {
  virtualisation = {
    podman.enable = true;
    podman.extraPackages = [ pkgs.zfs ];
    podman.dockerCompat = true;
  };

  virtualisation.containers.containersConf.settings = {
    containers.dns_servers = [
      "8.8.8.8"
      "8.8.4.4"
    ];
  };
  virtualisation.containers.storage.settings = {
    storage.driver = "zfs";
    storage.graphroot = "/var/lib/containers/storage";
    storage.runroot = "/run/containers/storage";
    storage.options.zfs.fsname = "zroot/podman";
  };
}
