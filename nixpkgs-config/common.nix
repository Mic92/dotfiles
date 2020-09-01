{ pkgs, config, lib, ... }:

{
  options.python.packages = lib.mkOption {
    type = lib.mkOptionType {
      name = "function";
      check = x: lib.isFunction x;
      merge = loc: functions: packageset:
        lib.foldl (list: function: list ++ (function packageset)) []
          (lib.getValues functions);
    };
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
    python.packages = ps: [
      ps.pyls-mypy
      ps.pyls-isort
      ps.pyls-black
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

      # python language server + plugins
      (pkgs.python3.withPackages (ps: config.python.packages ps))
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
      ripgrep
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

    home.stateVersion = "20.09";
    programs.home-manager.enable = true;
  };
}
