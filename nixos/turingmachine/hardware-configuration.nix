{ config
, lib
, pkgs
, modulesPath
, ...
}:
#let
#  frameworkFirmware = pkgs.stdenv.mkDerivation rec {
#    pname = "framework-firmware-update";
#    version = "3.17";
#    src = pkgs.fetchzip {
#      url = "https://downloads.frame.work/bios/Framework_Laptop_11th_gen_Intel_Core_BIOS_${version}_EFI.zip";
#      sha256 = "sha256-AeKlRCH8Np7OMxMbEI6pHd8OXfK4pHJTMK8FY0Y6uhs=";
#      stripRoot = false;
#    };
#    installPhase = ''
#      mkdir $out
#      cp H2OFFT-Sx64.efi hx20_capsule_${version}.bin startup.nsh $out
#    '';
#  };
#in
{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];

  # XXX: this only works with systemd-boot, not with lanzaboote
  #boot.loader.systemd-boot.extraFiles = {
  #  "H2OFFT-Sx64.efi" = "${frameworkFirmware}/H2OFFT-Sx64.efi";
  #  "startup.nsh" = "${frameworkFirmware}/startup.nsh";
  #  "hx20_capsule_${frameworkFirmware.version}.bin" = "${frameworkFirmware}/hx20_capsule_${frameworkFirmware.version}.bin";
  #  "efi/edk2/shell.efi" = "${pkgs.edk2-uefi-shell}/shell.efi";
  #};

  # When running this, have a usb stick installer at your hand as it will reset all your NVRAM and boot loader entries!
  #boot.loader.systemd-boot.extraEntries."framework-firmware-upgrade.conf" = ''
  #  title  Framwork Firmware Upgrade
  #  efi    /efi/edk2/shell.efi
  #  options startup.nsh
  #'';

  services.fwupd.enable = true;
  #services.fwupd.extraRemotes = [ "lvfs-testing" ];
  #services.fwupd.uefiCapsuleSettings.DisableCapsuleUpdateOnDisk = true;

  services.thermald.enable = true;

  # framework
  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "thunderbolt" ];
  boot.kernelModules = [ "kvm-intel" ];

  # for zfs
  networking.hostId = "8425e349";

  hardware = {
    bluetooth.enable = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  # for pactl
  environment.systemPackages = [ pkgs.pulseaudio ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;


  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
