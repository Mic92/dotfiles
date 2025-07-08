{
  pkgs,
  self,
  ...
}:
{
  home.packages = [
    self.packages.${pkgs.system}.claude-code
    self.packages.${pkgs.system}.claude-md
    pkgs.pueue
  ];
}
