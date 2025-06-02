{
  perSystem =
    { pkgs, ... }:
    {
      packages =
        {
          mergify-gen-config = pkgs.python3.pkgs.callPackage ./mergify-gen-config { };
          merge-when-green = pkgs.callPackage ./merge-when-green { };
        }
        // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          sengi = pkgs.callPackage ./sengi { };
        };
    };
}
