{ pkgs, self, ... }:
{
  imports = [ ../../../nixosModules/packages.nix ];

  environment.systemPackages = with pkgs; [
    cntr
    ntfs3g
    sunshine
    self.packages.${pkgs.stdenv.hostPlatform.system}.sengi
  ];
}
