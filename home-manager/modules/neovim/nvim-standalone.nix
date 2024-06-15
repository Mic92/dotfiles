{
  writeShellScriptBin,
  nvim-lsp-packages,
  treesitter-grammars,
  vimPlugins,
  neovim,
  nvim-appname,
  buildEnv,
}:
let
  lspEnv = buildEnv {
    name = "lsp-servers";
    paths = nvim-lsp-packages;
  };
in
writeShellScriptBin "nvim" ''
  set -efux
  unset VIMINIT
  export PATH=${lspEnv}/bin:$PATH
  export NVIM_APPNAME=${nvim-appname}

  XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
  XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}

  mkdir -p "$XDG_CONFIG_HOME/$NVIM_APPNAME" "$XDG_DATA_HOME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  rm -rf "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  cp -arfT '${../../../home/.config/nvim}'/ "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  ${neovim}/bin/nvim --headless -c 'quitall' # install plugins
  mkdir -p "$XDG_DATA_HOME/$NVIM_APPNAME/lib/" "$XDG_DATA_HOME/$NVIM_APPNAME/site/"
  ln -sfT "${vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so" "$XDG_DATA_HOME/$NVIM_APPNAME/lib/libfzf.so"
  ln -sfT "${treesitter-grammars}" "$XDG_DATA_HOME/$NVIM_APPNAME/site/parser"
  echo "${treesitter-grammars.rev}" > "$XDG_CONFIG_HOME/$NVIM_APPNAME/treesitter-rev"

  exec ${neovim}/bin/nvim "$@"
''
