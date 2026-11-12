{
  pkgs,
  lib,
  self,
  ...
}:
let
  uci = pkgs.callPackage ./nix { };
  retiolumHosts = import (self.inputs.kartei + "/modules/retiolum/hosts.nix") { inherit lib; };
in
{
  packages.openwrt-rauter =
    (uci.writeUci {
      imports = [ ./example.nix ];
      _module.args = {
        inherit retiolumHosts;
        inherit (self.inputs) kartei;
      };
    }).deploy;

  devShells.openwrt = pkgs.mkShell {
    buildInputs = [
      pkgs.just
      pkgs.sops
    ];
  };
}
