# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./modules/sshd.nix
    ./modules/telegraf.nix

    ../modules/hass-agent.nix
    ../modules/packages.nix
    ../modules/powertop.nix
    ../modules/users.nix
    ../modules/zfs.nix
    ../modules/tracing.nix
  ];
  networking.retiolum = {
    ipv4 = "10.243.29.169";
    ipv6 = "42:0:3c46:1452:ca55:fdaf:b12b:3027";
  };

  boot.zfs.requestEncryptionCredentials = [ "zroot/root" ];
  boot.loader.systemd-boot.enable = true;

  users.extraUsers.shannan = {
    isNormalUser = true;
    home = "/home/shannan";
    extraGroups = [ "wheel" "plugdev" "adbusers" "input" "kvm" ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1001;
  };
  networking.hostName = "bernie";
  networking.hostId = "ac174b52";

  networking.networkmanager.enable = true;

  time.timeZone = null;
  services.geoclue2.enable = true;
  i18n.defaultLocale = "en_DK.UTF-8";

  programs.vim.defaultEditor = true;

  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome3.enable = true;
  };

  programs.java.enable = true;
  programs.java.package = pkgs.jdk15_headless;

  environment.systemPackages = with pkgs; [
    firefox-wayland
    chromium
    celluloid
    vscode
    mpv
    youtube-dl
    calibre
    libreoffice
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    ferdi
    xournalpp
    zoom-us
    calibre
    evolution
    signal-desktop
    gimp
    wl-clipboard
    poppler_utils
    focuswriter
    jetbrains.idea-community
  ];

  documentation.doc.enable = false;

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.gutenprint ];
  };
}
