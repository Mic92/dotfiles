{
  ...
}:
{
  perSystem =
    {
      pkgs,
      ...
    }:
    let
      uci = pkgs.callPackage ./nix { };
    in
    {
      packages.openwrt-example = (uci.writeUci ./example.nix).command;

      devShells.openwrt = pkgs.mkShell {
        buildInputs = [
          pkgs.just
          pkgs.sops
        ];
      };
    };
}
