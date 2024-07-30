{ self, inputs, ... }:
{
  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    inputs.srvos.darwinModules.common
    inputs.srvos.darwinModules.mixins-telegraf
    inputs.srvos.darwinModules.mixins-terminfo
    inputs.srvos.darwinModules.mixins-nix-experimental
    ../modules/nix-daemon.nix
    ../modules/homebrew.nix
    ../modules/iterm2.nix
    ../modules/nix-daemon.nix
    ../modules/nix-index.nix
    ../modules/openssh.nix
    ../modules/secretiv.nix
    ../modules/sudo.nix
  ];

  # fix vim repeat key
  system.activationScripts.defaults.text = ''
    defaults write -g ApplePressAndHoldEnabled -bool false
  '';

  programs.zsh.enable = true;

  srvos.flake = self;
}
