{ pkgs, config, lib, ... }:

{
  imports = [
    #./modules/neovim.nix
    ./modules/emacs
  ];

  config = {
    home.packages = with pkgs; [
      #(pkgs.callPackage /home/joerg/git/nix-review {})
      nur.repos.mic92.nixpkgs-review-unstable
      nur.repos.mic92.mosh-ssh-agent
      nix-prefetch

      gdb
      hexyl
      binutils
      clang-tools
      nixpkgs-fmt
      shfmt
      emacsGcc

      python3.pkgs.black
      pyright

      dua
      nixFlakes
      tmux
      htop
      hub
      tig
      lazygit
      delta
      scc
      xcp
      direnv
      (nix-direnv.override { enableFlakes = true; })
      fzf
      exa
      zoxide
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
      mcfly
    ] ++ (lib.optionals pkgs.stdenv.isLinux [
      strace
      psmisc
      glibcLocales
    ]);

    home.stateVersion = "20.09";
    home.username = "joerg";
    home.homeDirectory = "/home/joerg";
    programs.home-manager.enable = true;
  };
}
