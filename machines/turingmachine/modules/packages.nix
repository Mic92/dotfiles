{ pkgs, ... }:
{
  imports = [ ../../../nixosModules/packages.nix ];

  environment.systemPackages = with pkgs; [
    cntr
    ntfs3g
    sunshine
  ];
}
