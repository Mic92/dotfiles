{
  config,
  pkgs,
  self,
  ...
}:
let
  inherit (self.packages.${pkgs.hostPlatform.system}) neovim treesitter-grammars;
  inherit (self.legacyPackages.${pkgs.hostPlatform.system}) nvim-lsp-packages;
in
{
  home.packages = nvim-lsp-packages ++ [ neovim ];
  # treesitter-grammars
  xdg.dataFile."nvim/site/parser".source = treesitter-grammars;

  home.activation.nvim = ''
    echo "${treesitter-grammars.rev}" > "${config.xdg.configHome}/nvim/treesitter-rev"
    XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
    NVIM_APPNAME=''${NVIM_APPNAME:-nvim}
    if [[ -f $XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json ]]; then
      if ! grep -q "${treesitter-grammars.rev}" "$XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json"; then
        nvim --headless "+Lazy! update" +qa
      fi
    fi
  '';

  xdg.dataFile."nvim/lib/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
}
