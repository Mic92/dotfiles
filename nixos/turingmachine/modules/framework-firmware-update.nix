{ pkgs, ... }:

let
  frameworkFirmware = pkgs.stdenv.mkDerivation rec {
    pname = "framework-firmware-update";
    version = "3.10";
    src = pkgs.fetchzip {
      url = "https://downloads.frame.work/bios/Framework_Laptop_11th_gen_Intel_Core_BIOS_${version}_EFI.zip";
      sha256 = "sha256-hAwwaJQ6hFfn1Zs26ijEQq2SjPk4op4zjjT0/Wt+/MQ=";
      stripRoot = false;
    };
    installPhase = ''
      mkdir $out
      cp H2OFFT-Sx64.efi hx20_capsule_${version}.bin startup.nsh $out
    '';
  };
in
{
  boot.loader.systemd-boot.extraFiles = {
    "H2OFFT-Sx64.efi" = "${frameworkFirmware}/H2OFFT-Sx64.efi";
    "startup.nsh" = "${frameworkFirmware}/startup.nsh";
    "hx20_capsule_${frameworkFirmware.version}.bin" = "${frameworkFirmware}/hx20_capsule_${frameworkFirmware.version}.bin";
    "efi/edk2/shell.efi" = "${pkgs.edk2-uefi-shell}/shell.efi";
  };

  # When running this, have a usb stick installer at your hand as it will reset all your NVRAM and boot loader entries!
  boot.loader.systemd-boot.extraEntries."framework-firmware-upgrade.conf" = ''
    title  Framwork Firmware Upgrade
    efi    /efi/edk2/shell.efi
    options startup.nsh
  '';
}
