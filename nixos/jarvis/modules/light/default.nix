{ pkgs, ... }: {
  users.extraUsers = {
    light = {
      isNormalUser = true;
      shell = "/run/current-system/sw/bin/bash";
      uid = 1042;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEVq+MusQOxc0o9VQv/cQbIH76I4/bJRz7g6sMZ1myuo hass@eve"
      ];
    };
  };
  hardware.bluetooth.enable = true;

  environment.systemPackages = [
    (pkgs.callPackage ./hue-bt-ctl.nix { })
  ];
}
