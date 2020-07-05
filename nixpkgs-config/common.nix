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
    #./modules/vim.nix
    ./modules/emacs
  ];

  config = {
    python.packages = [
      "pyls-mypy"
      "pyls-isort"
    ];

    home.packages = with pkgs; [
      #(pkgs.callPackage /home/joerg/git/nix-review {})
      nur.repos.mic92.nixpkgs-review-unstable
      nur.repos.mic92.mosh-ssh-agent
      nix-prefetch

      gdb
      hexyl
      binutils
      clang-tools
      grc

      # python language server + plugins
      (pkgs.python3.withPackages (ps: lib.attrVals config.python.packages ps))
      #rls

      nixFlakes
      tmux
      htop
      gitAndTools.hub
      gitAndTools.tig
      tokei
      direnv
      nix-direnv
      fzf
      exa
      pinentry
      fd
      bat
      vivid
      silver-searcher
      zsh
      less
      bashInteractive
      gnupg
      ncurses
      coreutils
      git
    ] ++ (lib.optionals pkgs.stdenv.isLinux [
      strace
      psmisc
      glibcLocales
    ]);

    home.stateVersion = "18.09";
    programs.home-manager.enable = true;
  };
}
