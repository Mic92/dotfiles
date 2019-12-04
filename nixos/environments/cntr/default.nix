with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = with rustPackages; [
    python3
    clippy
    rustc
    rls
  ];
}
