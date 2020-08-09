{ pkgs ? import <nixpkgs> {} }:

mkShell {
  DRONE_SERVER = "https://drone.thalheim.io";
  nativeBuildInputs = [
    pkgs.drone-cli
    # is this a good idea?
    #pkgs.ansible
    pkgs.sops
  ];
}
