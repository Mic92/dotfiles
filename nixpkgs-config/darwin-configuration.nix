{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    pkgs.vim
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.package = pkgs.nix;
  nix.trustedUsers = [ "joerg" ];
  # https://github.com/NixOS/nix/issues/719
  nix.extraOptions = ''
    builders-use-substitutes = true
    keep-outputs = true
    keep-derivations = true
  '';
  nix.nixPath = [
    "darwin=${builtins.getEnv "HOME"}/git/nix-darwin"
  ];

  environment.shells = [ pkgs.zsh ];
  environment.darwinConfig = "${builtins.getEnv "HOME"}/.config/nixpkgs/darwin-configuration.nix";

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
