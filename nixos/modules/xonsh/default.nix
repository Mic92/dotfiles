{
  self,
  pkgs,
  ...
}: {
  programs.xonsh.enable = true;
  environment.systemPackages = [
    self.packages.${pkgs.system}.xonsh
  ];
}
