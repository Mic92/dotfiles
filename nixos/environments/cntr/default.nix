with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    rustc
    cargo
    rustup
  ];
  hardeningDisable = [ "format" ];
  DUMP_OUTPUT="true";
}
