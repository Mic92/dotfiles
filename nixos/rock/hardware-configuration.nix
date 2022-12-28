{ lib
, modulesPath
, ...
}: {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];

  boot.kernelParams = [
    "earlycon=uart8250,mmio32,0xff130000"
    "coherent_pool=1M"
    "ethaddr=\${ethaddr}"
    "eth1addr=\${eth1addr}"
    "serial=\${serial#}"

    # The last console gets the systemd status messages.
    # Assume more people will find HDMI more useful than serial.
    "console=uart8250,mmio32,0xff130000"
    "console=tty1"
  ];

  services.udev.extraRules = ''
    KERNEL=="mali", MODE="0660", GROUP="video"
    KERNEL=="rkvdec", MODE="0660", GROUP="video"
    KERNEL=="vpu-service", MODE="0660", GROUP="video"
  '';

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
    fsType = "ext4";
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
}
