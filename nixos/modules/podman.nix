{ lib
, pkgs
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

  # dual-stack containers
  environment.etc."cni/net.d/87-podman-bridge.conflist".source = lib.mkForce (builtins.toFile "87-podman-bridge.conflist" (builtins.toJSON {
    cniVersion = "0.4.0";
    name = "podman";
    plugins = [
      {
        type = "bridge";
        bridge = "cni-podman0";
        isGateway = true;
        ipMasq = true;
        hairpinMode = true;
        ipam = {
          type = "host-local";
          routes = [
            { dst = "0.0.0.0/0"; }
            { dst = "::/0"; }
          ];
          ranges = [
            [
              {
                subnet = "10.88.0.0/16";
                gateway = "10.88.0.1";
              }
            ]
            [
              {
                subnet = "fd42:4492:6a6d:0041::/64";
                rangeStart = "fd42:4492:6a6d:0041::0";
                rangeEnd = "fd42:4492:6a6d:0041::ffff";
              }
            ]
          ];
        };
      }
      {
        type = "portmap";
        capabilities = {
          portMappings = true;
        };
      }
      {
        type = "firewall";
      }
      {
        type = "tuning";
      }
    ];
  }));
}
