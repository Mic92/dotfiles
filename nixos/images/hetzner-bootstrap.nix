# configuration.nix
# add
# imports = [ (builtins.fetchGit { url = "https://github.com/Mic92/dotfiles"; }) + "/nixos/images/hetzner-bootstrap.nix"; ];
# $ nix-shell -p git --command 'nixos-install'
{ ... }: {
  imports = [
    ./base-config.nix
    ./zfs.nix
  ];
}
