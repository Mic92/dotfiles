{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        mergify-gen-config = pkgs.python3.pkgs.callPackage ./mergify-gen-config { };
      };
    };
}
