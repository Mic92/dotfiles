{ lib
, config
, pkgs
, ...
}: {
  services.k3s.enable = true;
  services.k3s.docker = lib.mkForce false;

  # We only want k3s to manage firewalls or else we have to duplicate firewall
  # management.
  networking.firewall.enable = false;

  sops.secrets.k3s-server-token = { };
  services.k3s.tokenFile = config.sops.secrets.k3s-server-token.path;

  virtualisation.containerd.enable = true;
  virtualisation.containerd.settings = {
    version = 2;
    plugins."io.containerd.grpc.v1.cri" = {
      cni.conf_dir = "/var/lib/rancher/k3s/agent/etc/cni/net.d/";
      # FIXME: upstream
      cni.bin_dir = "${
        pkgs.runCommand "cni-bin-dir" {} ''
          mkdir -p $out
          ln -sf ${pkgs.cni-plugins}/bin/* ${pkgs.cni-plugin-flannel}/bin/* $out
        ''
      }";
    };
  };

  systemd.services.containerd.serviceConfig = lib.mkIf config.boot.zfs.enabled {
    ExecStartPre = [
      "-${pkgs.zfs}/bin/zfs create -o mountpoint=/var/lib/containerd/io.containerd.snapshotter.v1.zfs zroot/containerd"
      "-${pkgs.zfs}/bin/zfs mount zroot/containerd"
    ];
  };
  systemd.services.k3s = {
    wants = [ "containerd.service" ];
    after = [ "containerd.service" ];
  };
}
