{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  DRONE_SERVER = "https://drone.thalheim.io";
  nativeBuildInputs = [
    pkgs.sops
    pkgs.drone-cli
    # is this a good idea?
    #pkgs.ansible
  ];
}
