{ lib, ... }: {
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };
}
