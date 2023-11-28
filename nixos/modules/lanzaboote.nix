{ lib, ... }: {
  boot.loader.systemd-boot.enable = true;
  #boot.lanzaboote = {
  #  enable = true;
  #  pkiBundle = "/etc/secureboot";
  #};
}
