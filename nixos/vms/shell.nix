with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  inherit (nixops) buildInputs propagatedBuildInputs;
}
