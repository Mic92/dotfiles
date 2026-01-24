{
  pkgs,
  lib,
  config,
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    self.inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series
    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
    self.inputs.disko.nixosModules.disko
    self.inputs.srvos.nixosModules.desktop

    #self.inputs.spora.nixosModules.spora

    ./modules/caddy.nix
    ./modules/data-mesher.nix
    ./modules/disko.nix
    ./modules/networkmanager
    ./modules/nfs.nix
    ./modules/packages.nix
    ./modules/postgresql.nix
    ./modules/spotifyd.nix
    ../../nixosModules/tum-vpn
    ./modules/toggle-keyboard
    ./modules/sunshine.nix
    ./modules/ghaf-hardware.nix
    ./modules/ghaf-facter.nix

    ../../nixosModules/workstation.nix
    #../../nixosModules/qtile.nix
    #../../nixosModules/keyd.nix
    ../../nixosModules/ksmbd.nix
    #../../nixosModules/lanzaboote.nix
    ../../nixosModules/make-linux-fast.nix
    ../../nixosModules/nncp.nix
    ../../nixosModules/no-hz.nix
    ../../nixosModules/sshd/tor.nix
    ../../nixosModules/suspend-on-low-power.nix
  ];

  services.openssh.settings.PasswordAuthentication = lib.mkForce true;

  documentation.man.generateCaches = true;

  services.dbus.implementation = "broker";

  networking.nftables.enable = true;

  # Disabled: using custom async direnv integration in zshrc
  # programs.direnv.enable = true;

  hardware.graphics.enable32Bit = config.hardware.graphics.enable;

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  environment.systemPackages = with pkgs; [
    rustdesk
    sunshine
  ];

  hardware.saleae-logic.enable = true;

  services.ollama.enable = true;

  services.pcscd.enable = true;

  # https://community.frame.work/t/guide-linux-battery-life-tuning/6665
  #services.tlp.enable = true;
  #services.tlp.settings."PCIE_ASPM_ON_BAT" = "powersupersave";

  # For the future: enable lc3
  # https://gitlab.freedesktop.org/pipewire/pipewire/-/wikis/LE-Audio-+-LC3-support
  #hardware.bluetooth.settings = {
  #  General = {
  #    ControllerMode = "le";
  #    Experimental = true;
  #    KernelExperimental = true;
  #  };
  #};

  #environment.sessionVariables.KWIN_DRM_ALLOW_INTEL_COLORSPACE = "1";
  #services.displayManager.environment.KWIN_DRM_ALLOW_INTEL_COLORSPACE = "1";

  networking.hostName = "turingmachine";

  virtualisation = {
    #anbox.enable = true;
    #lxc.enable = true;
    #lxd.enable = true;
    #rkt.enable = true;
  };

  networking.firewall.allowedTCPPorts = [ 8081 ];

  system.stateVersion = "23.11";

  services.ksmbd.enable = true;
  services.ksmbd.openFirewall = true;
  services.ksmbd.shares.public = {
    path = "/var/lib/ksmbd";
    "read only" = true;
    browseable = "yes";
    "guest ok" = "yes";
    comment = "Public samba share.";
  };
}
