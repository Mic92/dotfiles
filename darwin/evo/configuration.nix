{
  self,
  inputs,
  pkgs,
  ...
}:
{
  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    inputs.srvos.darwinModules.common
    inputs.srvos.darwinModules.mixins-telegraf
    inputs.srvos.darwinModules.mixins-terminfo
    inputs.srvos.darwinModules.mixins-nix-experimental
    inputs.sops-nix.darwinModules.sops
    ../modules/app-store
    ../modules/clan/default.nix
    ../modules/homebrew.nix
    ../modules/iterm2.nix
    ../modules/nix-daemon.nix
    ../modules/nix-index.nix
    ../modules/openssh.nix
    ../modules/remote-builder.nix
    ../modules/secretiv.nix
    ../modules/sudo.nix
    ../modules/ghostty.nix
  ];

  clan.core.settings.directory = ../..;
  clan.core.settings.machine.name = "evo";
  sops.age.keyFile = "/Library/Application Support/sops-nix/age-key.txt";

  # fix vim repeat key
  system.activationScripts.defaults.text = ''
    defaults write -g ApplePressAndHoldEnabled -bool false
  '';

  environment.systemPackages = [
    pkgs.python3
  ];

  programs.zsh.enable = true;

  system.stateVersion = 5;

  srvos.flake = self;
}
