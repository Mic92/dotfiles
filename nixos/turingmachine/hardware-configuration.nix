{
  lib,
  pkgs,
  modulesPath,
  ...
}:
#let
#  frameworkFirmware = pkgs.stdenv.mkDerivation rec {
#    pname = "framework-firmware-update";
#    version = "3.05";
#    src = pkgs.fetchzip {
#      url = "https://downloads.frame.work/bios/Framework_Laptop_13_13th_Gen_Intel_Core_BIOS__${version}_EFI.zip";
#      sha256 = "sha256-AeKlRCH8Np7OMxMbEI6pHd8OXfK4pHJTMK8FY0Y6uh0=";
#      stripRoot = false;
#    };
#    installPhase = ''
#      mkdir $out
#      cp H2OFFT-Sx64.efi hx20_capsule_${version}.bin startup.nsh $out
#    '';
#  };
#in
{
  imports = [ "${modulesPath}/installer/scan/not-detected.nix" ];

  # XXX: this only works with systemd-boot, not with lanzaboote
  #boot.loader.systemd-boot.extraFiles = {
  #  "H2OFFT-Sx64.efi" = "${frameworkFirmware}/H2OFFT-Sx64.efi";
  #  "startup.nsh" = "${frameworkFirmware}/startup.nsh";
  #  "hx20_capsule_${frameworkFirmware.version}.bin" = "${frameworkFirmware}/hx20_capsule_${frameworkFirmware.version}.bin";
  #  "efi/edk2/shell.efi" = "${pkgs.edk2-uefi-shell}/shell.efi";
  #};

  ## When running this, have a usb stick installer at your hand as it will reset all your NVRAM and boot loader entries!
  #boot.loader.systemd-boot.extraEntries."framework-firmware-upgrade.conf" = ''
  #  title  Framwork Firmware Upgrade
  #  efi    /efi/edk2/shell.efi
  #  options startup.nsh
  #'';

  services.fwupd.enable = true;
  #services.fwupd.extraRemotes = [ "lvfs-testing" ];
  #services.fwupd.uefiCapsuleSettings.DisableCapsuleUpdateOnDisk = true;

  services.thermald.enable = true;

  # for zfs
  networking.hostId = "8425e349";

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # for pactl
  environment.systemPackages = [ pkgs.pulseaudio ];

  #hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
