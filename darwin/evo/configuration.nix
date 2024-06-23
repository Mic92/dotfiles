{ self, inputs, ... }:
{
  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    inputs.srvos.darwinModules.common
    inputs.srvos.darwinModules.mixins-telegraf
    ../modules/nix-daemon.nix
    ../modules/amethyst.nix
    ../modules/homebrew.nix
    ../modules/secretiv.nix
    ../modules/firefox.nix
    ../modules/iterm2.nix
    ../modules/openssh.nix
    ../modules/sudo.nix
  ];


  srvos.flake = self;
}
