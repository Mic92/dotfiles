{ pkgs
, config
, lib
, ...
}: {
  imports = [
    ./modules/neovim
    #./modules/emacs
  ];

  config = {
    home.packages = with pkgs;
      [
        #(pkgs.callPackage /home/joerg/git/nix-review {})
        config.nur.repos.mic92.nixpkgs-review-unstable
        config.nur.repos.mic92.tmux-thumbs
        nix-prefetch

        hexyl
        binutils
        clang-tools
        nixpkgs-fmt
        shfmt
        terraform-ls
        ouch

        python3.pkgs.black
        pyright
        nil

        du-dust
        pkgs.nix

        socat
        tmux
        nurl
        htop
        hub
        gh
        hyperfine
        jq
        yq-go
        tig
        lazygit
        git-absorb
        delta
        scc
        direnv
        (nix-direnv.override { enableFlakes = true; })
        fzf
        lsd
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
        gdb
      ]);

    home.enableNixpkgsReleaseCheck = false;

    home.stateVersion = "20.09";
    home.username = "joerg";
    home.homeDirectory = "/home/joerg";
    programs.home-manager.enable = true;
  };
}
