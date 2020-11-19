 # Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
{
  imports = [
    ../modules/packages.nix
    ../modules/users.nix
    ../modules/zfs.nix
    ./modules/networkmanager.nix
  ];

  users.extraUsers.shannan = {
    isNormalUser = true;
    home = "/home/joerg";
    extraGroups = [ "wheel" "plugdev" "adbusers" "input" "kvm" ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1001;
  };
  networking.hostName = "bernie";
  networking.hostId = "ac174b52";

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  time.timeZone = null;
  i18n.defaultLocale = "en_DK.UTF-8";

  powerManagement.powertop.enable = true;
  programs.vim.defaultEditor = true;

  services.xserver.desktopManager.gnome3.enable = true;
  documentation.doc.enable = false;

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    browsing = true;
  };
}
