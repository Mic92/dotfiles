# nix-build '<nixpkgs/nixos>' -A config.system.build.sdImage -I nixos-config=./rpi3-image.nix

{ ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix>
    ./base-config.nix
  ];
}
