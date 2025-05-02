{
  perSystem =
    { pkgs, lib, ... }:
    {
      devShells = lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
        ldap = pkgs.mkShell {
          packages = [
            pkgs.apache-directory-studio
          ];
        };
      };
    };
}
