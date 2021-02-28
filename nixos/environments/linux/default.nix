with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    bashInteractive
    #scan-build
    bear
    ncurses
    gcc7
    sqlite
  ] ++ linux.buildInputs ++ linux.nativeBuildInputs;

  hardeningDisable = [ "bindnow" "format" "fortify" "stackprotector" "pic" ];
}
