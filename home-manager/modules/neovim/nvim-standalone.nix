{
  writeShellScriptBin,
  nvim-lsp-packages,
  nvim-install-treesitter,
  neovim,
  nvim-appname,
  lua-config ? ../../../home/.config/nvim,
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
  export PATH=${lspEnv}/bin:${neovim}/bin:$PATH
  export NVIM_APPNAME=${nvim-appname}

  XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
  XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}

  mkdir -p "$XDG_CONFIG_HOME/$NVIM_APPNAME" "$XDG_DATA_HOME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  rm -rf "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  cp -arfT '${lua-config}'/ "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"

  # lock file is not in sync with treesitter-rev, force update of lazy-lock.json
  if ! grep -q "${nvim-install-treesitter.rev}" "$XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json"; then
    echo "${nvim-install-treesitter.rev}" > "$XDG_CONFIG_HOME/$NVIM_APPNAME/treesitter-rev"
    # annoyingly we would run this on every nvim invocation again because we overwrite the lock file
    nvim --headless "+Lazy! update" +qa
  else
    nvim --headless -c 'quitall' # install plugins, if needed
  fi

  exec nvim "$@"
''
