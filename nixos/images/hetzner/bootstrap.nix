# configuration.nix
# add
# $ nixos-generate-config
# imports = [ ((builtins.fetchGit { url = "https://github.com/Mic92/dotfiles" }) + "/nixos/images/hetzner/bootstrap.nix") ];
# $ nix-shell -p git --command 'nixos-install'
{ lib, ... }: {
  imports = [
    ./base.nix
  ];

  boot.initrd.network.ssh = {
    port = lib.mkForce 22222;
    hostECDSAKey = lib.mkForce null;
  };

  boot.loader.grub.devices = [ "/dev/sda" "/dev/sdb" ];

  # make our configuration locally evaluate
  #fileSystems."/" = {
  #  device = "zroot/root/nixos";
  #  fsType = "zfs";
  #};
}
