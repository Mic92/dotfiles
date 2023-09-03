{ writeShellScriptBin
, astro-nvim-config
, buildEnv
, vimPlugins
, neovim
, nvim-appname
}:
writeShellScriptBin "nvim" ''
  set -efux
  unset VIMINIT
  export PATH=${buildEnv {
    name = "lsp-servers";
    paths = astro-nvim-config.lspPackages;
  }}/bin:$PATH
  export NVIM_APPNAME=${nvim-appname}

  mkdir -p $HOME/.config $HOME/.data/
  ln -sfT ${astro-nvim-config} "$HOME"/.config/$NVIM_APPNAME
  ${neovim}/bin/nvim --headless -c 'quitall' # install plugins
  if [[ -d $HOME/.data/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim ]]; then
    mkdir -p "$HOME/.data/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim/build"
    ln -sf "${vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so" "$HOME/.data/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim/build/libfzf.so"
  fi
  exec ${neovim}/bin/nvim "$@"
''
