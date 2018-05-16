with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  #propagatedBuildInputs = nixops.propagatedBuildInputs;
  #buildInputs = [ nix ] ++ nixops.buildInputs;
  buildInputs = [
    nixops
  ];
}
