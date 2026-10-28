{
  lib,
  pkgs,
  self,
  ...
}:
{
  imports = [ self.inputs.iets.nixosModules.default ];

  # Puts iets's classic commands (nix-build, nix-shell, ...) in front of Nix.
  # Not via iets.classic.enable: that sets nix.package at the same priority
  # as nix-daemon.nix and would conflict.
  nix.package = lib.mkOverride 90 (
    self.inputs.iets.lib.mkClassic pkgs
      self.inputs.iets.packages.${pkgs.stdenv.hostPlatform.system}.default
      self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix
  );

  # iets loads no plugins and speaks grpc:// natively, so the tarmac and
  # nix-grpc-store plugins are dead weight here.
  programs.nix-grpc-store.enable = lib.mkForce false;
  nix.settings.plugin-files = lib.mkForce [ ];
}
