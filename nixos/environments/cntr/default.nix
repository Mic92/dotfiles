with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    rustc
    cargo
    rustup
    hello
  ];
  hardeningDisable = [ "format" ];
  DUMP_OUTPUT="true";
}
