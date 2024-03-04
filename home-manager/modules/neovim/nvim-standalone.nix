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

  XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
  XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}

  mkdir -p "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"
  ln -sfT ${astro-nvim-config} "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  ${astro-nvim-config.neovim}/bin/nvim --headless -c 'quitall' # install plugins
  if [[ -d $HOME/.data/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim ]]; then
    mkdir -p "$XDG_DATA_HOME/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim/build"
    ln -sf "${vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so" "$XDG_DATA_HOME/$NVIM_APPNAME/lazy/telescope-fzf-native.nvim/build/libfzf.so"
  fi
  exec ${astro-nvim-config.neovim}/bin/nvim "$@"
''
