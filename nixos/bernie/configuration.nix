# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./modules/sshd.nix

    ../modules/hass-agent.nix
    ../modules/ip-update.nix
    ../modules/iwd.nix
    ../modules/packages.nix
    ../modules/powertop.nix
    ../modules/promtail.nix
    ../modules/users.nix
    ../modules/zfs.nix
    ../modules/tracing.nix
    ../modules/telegraf.nix
    ../modules/pipewire.nix
  ];
  networking.retiolum.ipv6 = "42:0:3c46:1452:ca55:fdaf:b12b:3027";

  boot.zfs.requestEncryptionCredentials = [ "zroot/root" ];
  boot.zfs.enableUnstable = true;
  boot.loader.systemd-boot.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_zen;

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
  users.users.shannan.extraGroups = [ "networkmanager" ];

  time.timeZone = null;
  services.geoclue2.enable = true;
  i18n.defaultLocale = "en_DK.UTF-8";

  programs.vim.defaultEditor = true;

  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "gnome";
    desktopManager.gnome.enable = true;
  };

  environment.systemPackages = with pkgs; [
    (pkgs.callPackage (pkgs.fetchFromGitHub {
      owner = "Mic92";
      repo = "tts-app";
      rev = "0.0.1";
      sha256 = "sha256-J65Eoa6ua7jdmC8/veVfL5oP1IX5lC94EDxP9L59ufQ=";
    }) {
      defaultHost = "tts.r";
      defaultPort = "80";
    })
    firefox-wayland
    chromium
    celluloid
    vscode
    mpv
    inkscape
    youtube-dl
    calibre
    libreoffice
    ubuntu_font_family
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    zoom-us
    calibre
    evolution
    signal-desktop
    gimp
    wl-clipboard
    poppler_utils
    focuswriter
  ];

  documentation.doc.enable = false;

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.gutenprint ];
  };
  sops.defaultSopsFile = ./secrets/secrets.yaml;
}
