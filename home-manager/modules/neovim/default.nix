{
  config,
  pkgs,
  self,
  ...
}:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) neovim nvim-install-treesitter nvim-open;
  inherit (self.legacyPackages.${pkgs.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [
    neovim
    nvim-open
  ];

  home.activation.nvim = ''
    echo "${nvim-install-treesitter.rev}" > "${config.xdg.configHome}/nvim/treesitter-rev"
    XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
    NVIM_APPNAME=''${NVIM_APPNAME:-nvim}
    if [[ -f $XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json ]]; then
      if ! grep -q "${nvim-install-treesitter.rev}" "$XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json"; then
        ${neovim}/bin/nvim --headless "+Lazy! update" +qa
      fi
    fi
  '';
}
