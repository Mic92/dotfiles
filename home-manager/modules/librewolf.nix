{ pkgs, self, ... }:

let
  firefoxExtensions = pkgs.callPackages ../../pkgs/firefox-extensions { };
in
{
  home.packages =
    [
      firefoxExtensions.browser-cli-extension
      firefoxExtensions.chrome-tab-gc-extension
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.librewolf ]
    ++ pkgs.lib.optionals (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") [
      self.packages.${pkgs.stdenv.hostPlatform.system}.librewolf-macos
    ];
}
