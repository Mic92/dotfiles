{
  writeShellScriptBin,
  nvim-lsp-packages,
  neovim,
  nvim-appname,
  lua-config ? ../../../home/.config/nvim,
  buildEnv,
}:

# Create a combined environment with all LSP servers
let
  lspEnv = buildEnv {
    name = "lsp-servers";
    paths = nvim-lsp-packages;
  };
in
writeShellScriptBin "nvim" ''
  #!/usr/bin/env bash
  set -efu

  # Ensure clean environment for Neovim
  unset VIMINIT

  # Set up PATH to include LSP servers and Neovim
  export PATH=${lspEnv}/bin:${neovim}/bin:$PATH
  export NVIM_APPNAME=${nvim-appname}

  # Set up XDG directories if not already defined
  XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
  XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}

  # Prepare Neovim configuration directory
  config_dir="$XDG_CONFIG_HOME/$NVIM_APPNAME"
  mkdir -p "$config_dir" "$XDG_DATA_HOME"

  # Only re-copy config if the source is newer
  if [ ! -f "$config_dir/.source-hash" ] || \
     [ "$(cat "$config_dir/.source-hash" 2>/dev/null)" != '${lua-config}' ]; then
    chmod -R u+w "$config_dir" 2>/dev/null || true
    rm -rf "$config_dir"
    cp -arfT '${lua-config}'/ "$config_dir"
    chmod -R u+w "$config_dir"
    echo '${lua-config}' > "$config_dir/.source-hash"

    # Install plugins on first setup
    nvim --headless -c 'quitall'
  fi

  # Launch Neovim with all arguments passed to this script
  exec nvim "$@"
''
