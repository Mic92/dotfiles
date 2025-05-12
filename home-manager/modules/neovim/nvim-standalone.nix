{
  writeShellScriptBin,
  nvim-lsp-packages,
  nvim-install-treesitter,
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
  set -efux

  # Ensure clean environment for Neovim
  unset VIMINIT

  # Set up PATH to include LSP servers and Neovim
  export PATH=${lspEnv}/bin:${neovim}/bin:$PATH
  export NVIM_APPNAME=${nvim-appname}

  # Set up XDG directories if not already defined
  XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
  XDG_DATA_HOME=''${XDG_DATA_HOME:-$HOME/.local/share}

  # Prepare Neovim configuration directory
  mkdir -p "$XDG_CONFIG_HOME/$NVIM_APPNAME" "$XDG_DATA_HOME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  rm -rf "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  cp -arfT '${lua-config}'/ "$XDG_CONFIG_HOME/$NVIM_APPNAME"
  chmod -R u+w "$XDG_CONFIG_HOME/$NVIM_APPNAME"

  # Check if treesitter needs updating
  if ! grep -q "${nvim-install-treesitter.rev}" "$XDG_CONFIG_HOME/$NVIM_APPNAME/lazy-lock.json"; then
    echo "${nvim-install-treesitter.rev}" > "$XDG_CONFIG_HOME/$NVIM_APPNAME/treesitter-rev"
    # Update plugins with Lazy package manager
    nvim --headless "+Lazy! update" +qa
  else
    # Just check and install plugins if needed
    nvim --headless -c 'quitall'
  fi

  # Launch Neovim with all arguments passed to this script
  exec nvim "$@"
''
