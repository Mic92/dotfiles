with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    nixops
  ];
}
