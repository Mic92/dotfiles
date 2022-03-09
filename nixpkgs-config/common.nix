{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    #./modules/neovim.nix
    ./modules/emacs
  ];

  config = {
    home.packages = with pkgs;
      [
        #(pkgs.callPackage /home/joerg/git/nix-review {})
        nur.repos.mic92.nixpkgs-review-unstable
        nur.repos.mic92.mosh-ssh-agent
        nur.repos.mic92.tmux-thumbs
        (callPackage (fetchFromGitHub {
          owner = "nix-community";
          repo = "comma";
          rev = "02e3e5545b0c62595a77f3d5de1223c536af0614";
          sha256 = "sha256-WBIQmwlkb/GMoOq+Dnyrk8YmgiM/wJnc5HYZP8Uw72E=";
        }) {})
        nix-prefetch

        gdb
        hexyl
        binutils
        clang-tools
        (alejandra.overrideAttrs (old: {
          src = fetchFromGitHub {
            owner = "kamadorueda";
            repo = "alejandra";
            rev = "cc92e945d27c8b3a40060efd19839051e680033a";
            sha256 = "sha256-M35g176uRVBrIUHjztUZDn6c7MIhRObV1B/JgtK4cX8=";
          };
        }))
        shfmt

        python3.pkgs.black
        pyright

        du-dust
        pkgs.nixFlakes

        tmux
        htop
        hub
        tig
        lazygit
        git-absorb
        delta
        scc
        direnv
        (nix-direnv.override {enableFlakes = true;})
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
        ncurses
        coreutils
        git
      ]
      ++ (lib.optionals pkgs.stdenv.isLinux [
        strace
        psmisc
        glibcLocales
      ]);

    home.enableNixpkgsReleaseCheck = false;

    home.stateVersion = "20.09";
    home.username = "joerg";
    home.homeDirectory = "/home/joerg";
    programs.home-manager.enable = true;
  };
}
