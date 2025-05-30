{ self, ... }:
{
  imports = [ self.inputs.nix-index-database.darwinModules.nix-index ];
  programs.nix-index-database.comma.enable = true;
}
