with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    bashInteractive
    fuse
    meson
    ninja
    pkgconfig
    systemd
  ];
}
