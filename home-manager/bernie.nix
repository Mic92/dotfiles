{ ... }: {
  imports = [
    ./common.nix
  ];
  services.syncthing.enable = true;
}
