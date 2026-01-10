{
  pkgs,
  self,
  ...
}:
let
  inherit (self.packages.${pkgs.stdenv.hostPlatform.system})
    neovim
    nvim-open
    ;
  inherit (self.legacyPackages.${pkgs.stdenv.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [
    neovim
    nvim-open
  ];
}
