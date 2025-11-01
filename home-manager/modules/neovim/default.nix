{
  config,
  pkgs,
  self,
  ...
}:
let
  inherit (self.packages.${pkgs.stdenv.hostPlatform.system})
    neovim
    nvim-install-treesitter
    nvim-open
    nvim-treesitter-plugins
    ;
  inherit (self.legacyPackages.${pkgs.stdenv.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [
    neovim
    nvim-open
    nvim-treesitter-plugins
  ];

  home.activation.nvim = ''
    XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
    NVIM_APPNAME=''${NVIM_APPNAME:-nvim}
    if [[ -f $XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json ]]; then
      if ! grep -q "${nvim-install-treesitter.rev}" "$XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json"; then
        echo "${nvim-install-treesitter.rev}" > "${config.xdg.configHome}/nvim/treesitter-rev"
        ${neovim}/bin/nvim --headless "+Lazy! update" +qa
      fi
    fi
  '';
}
