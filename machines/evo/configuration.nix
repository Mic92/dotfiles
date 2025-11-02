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
    ../../darwinModules/homebrew.nix
    ../../darwinModules/nix-daemon.nix
    ../../darwinModules/nix-index.nix
    ../../darwinModules/openssh.nix
    ../../darwinModules/remote-builder.nix
    ../../darwinModules/secretiv.nix
    ../../darwinModules/sudo.nix
    ../../darwinModules/ghostty.nix
    ../../darwinModules/hyprspace.nix
    self.inputs.retiolum.darwinModules.tinc
    self.inputs.retiolum.darwinModules.retiolum
    self.inputs.retiolum.darwinModules.ca
    ../../nixosModules/tum-vpn
  ];

  services.hyprspace.enable = true;

  system.activationScripts.postActivation.text = ''
    # disable spotlight
    launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist >/dev/null 2>&1 || true
    # disable fseventsd on /nix volume
    mkdir -p /nix/.fseventsd
    test -e /nix/.fseventsd/no_log || touch /nix/.fseventsd/no_log
  '';

  fonts.packages = [ pkgs.nerd-fonts.fira-code ];

  sops.age.keyFile = "/Library/Application Support/sops-nix/age-key.txt";

  sops.secrets.test-secret = {
    owner = "joerg";
    path = "${config.users.users.joerg.home}/.foo";
    sopsFile = ./test-secrets.yml;
  };
  sops.templates."test-template.toml" = {
    content = ''
      password = "${config.sops.placeholder.test-secret}";
    '';
    uid = 501;
  };

  # fix vim repeat key
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  users.users.joerg.home = "/Users/joerg";

  environment.etc."nix-darwin".source = "${config.users.users.joerg.home}/.homesick/repos/dotfiles";

  environment.systemPackages = [
    pkgs.python3
    pkgs.nixos-rebuild
    pkgs.pinentry_mac
    self.packages.${pkgs.system}.blueutil
    self.packages.${pkgs.system}.systemctl-macos
    self.packages.${pkgs.system}.rbw-pinentry
  ];

  programs.zsh.enable = true;

  system.stateVersion = 5;

  srvos.flake = self;
}
