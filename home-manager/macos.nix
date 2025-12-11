{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/mail.nix
    ./modules/atuin-autosync.nix
    ./modules/firefox.nix
  ];
  home.packages =
    let
      self = inputs.self;
    in
    [
      pkgs.eternal-terminal
      pkgs.rbw
      inputs.strace-macos.packages.${pkgs.stdenv.hostPlatform.system}.default
      self.packages.${pkgs.stdenv.hostPlatform.system}.browser-cli
    ];
}
