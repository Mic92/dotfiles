{pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.03.tar.gz") {}}:
pkgs.stdenvNoCC.mkDerivation {
  name = "env";
  # jdk is broken in unstable
  nativeBuildInputs = [pkgs.apache-directory-studio];
}
