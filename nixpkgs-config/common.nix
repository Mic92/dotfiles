{ pkgs, config, lib, ... }:

{
  options.python.packages = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
    description = ''
      Names of python packages to install
    '';
  };

  imports = [
    ./vim.nix
    ./emacs.nix
  ];

  config = {
    python.packages = [
      "pyls-mypy"
      "pyls-isort"
      "pyls-black"
    ];

    home.packages = with pkgs; [
      #(pkgs.callPackage /home/joerg/git/nix-review {})
      nur.repos.mic92.nix-review-unstable

      gdb
      hexyl
      strace
      binutils
      ccls

      # python language server + plugins
      (pkgs.python3.withPackages (ps: lib.attrVals config.python.packages ps))

      tmux
      htop
      psmisc
      gitAndTools.hub
      gitAndTools.tig
      tokei
      mosh
      direnv
      fzf
      exa
      fd
      bat
      vivid
      silver-searcher
      zsh
      glibcLocales
      less
      bash
      gnupg
    ];

    home.stateVersion = "18.09";
  };
}
