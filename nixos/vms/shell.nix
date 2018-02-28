with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  propagatedBuildInputs = [ python2Packages.libvirt ] ++ nixops.propagatedBuildInputs;
  buildInputs = [ nix ] ++ nixops.buildInputs;
}
