# nix-build '<nixpkgs/nixos>' -A config.system.build.sdImage -I nixos-config=./rpi3-image.nix
{ ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";
  imports = [
    <nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix>
    ./base-config.nix
  ];
  networking.wireless.enable = false;
}
