with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    bashInteractive
    pass
  ];
  shellHook = ''
    export PASSWORD_STORE_DIR=$(readlink -f .)
  '';
}
