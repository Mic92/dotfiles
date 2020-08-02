{
  fileSystems."/var/lib/minidlna/nfs" = {
    device = "prism.r:/export/download";
    fsType = "nfs";
    options = [
      "timeo=14"
      "noauto"
      "noatime"
      "nodiratime"
      "noac"
      "nocto"
      "ro"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
      "x-systemd.requires=sys-devices-virtual-net-tinc.retiolum.device"
    ];
  };
}
