{
  self,
  pkgs,
  config,
  lib,
  ...
}:
{
  clan.core.networking.targetHost = lib.mkForce "root@evo.local";
  system.primaryUser = "joerg";

  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    self.inputs.srvos.darwinModules.common
    self.inputs.srvos.darwinModules.mixins-telegraf
    self.inputs.srvos.darwinModules.mixins-terminfo
    self.inputs.srvos.darwinModules.mixins-nix-experimental
    self.inputs.sops-nix.darwinModules.sops
    ../../darwinModules/app-store
    ../../darwinModules/nix-casks.nix
    ../../darwinModules/nix-daemon.nix
    ../../darwinModules/nix-index.nix
    ../../darwinModules/openssh.nix
    ../../darwinModules/remote-builder.nix
    ../../darwinModules/sudo.nix
    self.inputs.retiolum.darwinModules.tinc
    self.inputs.retiolum.darwinModules.retiolum
    self.inputs.retiolum.darwinModules.ca
    ../../nixosModules/tum-vpn
  ];

  system.activationScripts.postActivation.text = ''
    # disable spotlight
    launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist >/dev/null 2>&1 || true
    # disable fseventsd on /nix volume
    mkdir -p /nix/.fseventsd
    test -e /nix/.fseventsd/no_log || touch /nix/.fseventsd/no_log
  '';

  fonts.packages = [ pkgs.nerd-fonts.fira-code ];

  sops.age.keyFile = "/Library/Application Support/sops-nix/age-key.txt";

  # fix vim repeat key
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  users.users.joerg.home = "/Users/joerg";

  environment.etc."nix-darwin".source = "${config.users.users.joerg.home}/.homesick/repos/dotfiles";

  environment.systemPackages = [
    pkgs.python3
    pkgs.nixos-rebuild
    pkgs.pinentry_mac
    self.packages.${pkgs.stdenv.hostPlatform.system}.blueutil
    self.packages.${pkgs.stdenv.hostPlatform.system}.systemctl-macos
    self.packages.${pkgs.stdenv.hostPlatform.system}.rbw-pinentry
  ];

  programs.zsh.enable = true;

  system.stateVersion = 5;

  srvos.flake = self;
}
