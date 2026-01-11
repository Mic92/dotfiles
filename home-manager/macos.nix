{
  pkgs,
  inputs,
  self,
  lib,
  ...
}:
{
  imports = [
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/mail.nix
    ./modules/atuin-autosync.nix
    ./modules/librewolf.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
    pkgs.radicle-node
    inputs.strace-macos.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.niks3.packages.${pkgs.stdenv.hostPlatform.system}.niks3
    self.packages.${pkgs.stdenv.hostPlatform.system}.browser-cli
  ];

  home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ${self.packages.${pkgs.stdenv.hostPlatform.system}.browser-cli}/bin/browser-cli --install-host
  '';
}
