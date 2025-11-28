{ config, ... }:
{
  sops.secrets.shannan-password.neededForUsers = true;

  users.users.shannan = {
    isNormalUser = true;
    home = "/home/shannan";
    extraGroups = [
      "wheel"
      "plugdev"
      "adbusers"
      "input"
      "kvm"
      "networkmanager"
    ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1001;
    hashedPasswordFile = config.sops.secrets.shannan-password.path;
    inherit (config.users.users.joerg) openssh;
  };
}
