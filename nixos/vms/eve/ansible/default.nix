with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    python2Packages.lxc
  ];
}
