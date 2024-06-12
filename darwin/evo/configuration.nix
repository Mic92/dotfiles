{ self, inputs, ... }:
{
  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    inputs.srvos.darwinModules.common
    inputs.srvos.darwinModules.mixins-telegraf
    ../modules/nix-daemon.nix
    ../modules/homebrew.nix
    ../modules/secretiv.nix
  ];

  srvos.flake = self;
}
