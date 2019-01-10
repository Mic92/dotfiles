# configuration.nix
# add
# $ nixos-generate-config
# imports = [ ((builtins.fetchGit { url = "https://github.com/Mic92/dotfiles" }) + "/nixos/images/hetzner-bootstrap.nix") ];
# $ nix-shell -p git --command 'nixos-install'
{ ... }: {
  imports = [
    ./hetzner-base.nix
  ];
  boot.loader.grub.device = "/dev/sda";
}
