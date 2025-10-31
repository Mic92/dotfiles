# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{
  config,
  pkgs,
  self,
  lib,
  ...
}:
{
  imports = [
    ../../nixosModules/kde
    ../../nixosModules/packages.nix
    ../../nixosModules/powertop.nix
    ../../nixosModules/promtail.nix
    ../../nixosModules/users.nix
    ../../nixosModules/tracing.nix
    ../../nixosModules/pipewire.nix
    ../../nixosModules/hyprspace.nix
    ./disko.nix

    self.nixosModules.default
    self.inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
    self.inputs.home-manager.nixosModules.home-manager
    self.inputs.srvos.nixosModules.desktop
  ];

  # Disable envfs to fix systemd refusing to run with unpopulated /usr/
  services.envfs.enable = lib.mkForce false;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  boot.plymouth.enable = true;
  services.fwupd.enable = true;

  sops.secrets.dorit-password.neededForUsers = true;
  users.users.dorit.hashedPasswordFile = config.sops.secrets.dorit-password.path;

  clan.core.deployment.requireExplicitUpdate = true;

  boot.initrd.systemd.enable = true;

  i18n.defaultLocale = "de_DE.UTF-8";

  users.users.dorit = {
    isNormalUser = true;
    home = "/home/dorit";
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
    inherit (config.users.users.joerg) openssh;
  };

  networking.hostName = "dorits-laptop";

  networking.networkmanager.enable = true;
  time.timeZone = null;
  services.geoclue2.enable = true;

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
    bluetooth.enable = true;
  };

  environment.systemPackages = with pkgs; [
    celluloid
    mpv
    inkscape
    ubuntu-classic
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    gimp
    wl-clipboard
    poppler-utils
    libreoffice
    self.packages.${pkgs.stdenv.hostPlatform.system}.cewe-fotowelt
  ];
  fonts.enableDefaultPackages = true;

  documentation.doc.enable = false;
  documentation.man.enable = false;

  services.openssh.enable = true;
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.gutenprint ];
  };
  system.stateVersion = "24.11";
}
