# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  pkgs,
  self,
  ...
}:
{
  imports = [
    #../../nixosModules/gnome.nix
    ../../nixosModules/kde
    ../../nixosModules/ip-update.nix
    ../../nixosModules/packages.nix
    ../../nixosModules/powertop.nix
    ../../nixosModules/promtail.nix
    ../../nixosModules/users.nix
    ../../nixosModules/remote-builder.nix
    ../../nixosModules/tracing.nix
    ../../nixosModules/pipewire.nix
    ../../nixosModules/hyprspace.nix
    ../../nixosModules/samba-movieshare.nix
    ../../nixosModules/shannan.nix

    ./filesystems.nix

    self.nixosModules.default
    self.inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x13-intel
    self.inputs.home-manager.nixosModules.home-manager
    self.inputs.srvos.nixosModules.desktop
  ];

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  systemd.services.openssh = {
    before = [ "boot-complete.target" ];
    wantedBy = [ "boot-complete.target" ];
    unitConfig.FailureAction = "reboot";
  };

  boot.plymouth.enable = true;
  services.fwupd.enable = true;
  hardware.keyboard.qmk.enable = true;

  clan.core.deployment.requireExplicitUpdate = true;

  boot.initrd.systemd.enable = true;
  boot.loader.systemd-boot.enable = true;
  # when installing toggle this
  boot.loader.efi.canTouchEfiVariables = false;

  networking.hostName = "bernie";
  networking.hostId = "ac174b52";

  networking.networkmanager.enable = true;
  time.timeZone = null;
  services.geoclue2.enable = true;

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
    bluetooth.enable = true;
  };
  environment.systemPackages = with pkgs; [
    #(pkgs.callPackage (pkgs.fetchFromGitHub {
    #    owner = "Mic92";
    #    repo = "tts-app";
    #    rev = "0.0.2";
    #    sha256 = "sha256-QnGYwN3+Qul2jItK7NvKMc6rbtT+f1qXJF542s3EvTQ=";
    #  }) {
    #    defaultHost = "tts.r";
    #    defaultPort = "80";
    #  })
    firefox
    bottles
    geary
    teams-for-linux
    zoom-us
    #(retroarch.override {
    #  cores = [
    #    libretro.bsnes-hd
    #    libretro.mupen64plus
    #    libretro.beetle-psx-hw
    #  ];
    #})
    jellyfin-media-player
    chromium
    celluloid
    vscode
    mpv
    inkscape
    yt-dlp
    calibre
    ubuntu-classic
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    calibre
    signal-desktop
    gimp
    wl-clipboard
    poppler-utils
    focuswriter
    spicy
    virt-viewer
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
  system.stateVersion = "21.11";
}
