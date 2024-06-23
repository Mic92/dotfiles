{
  pkgs,
  config,
  lib,
  self,
  ...
}:
{
  imports = [
    ./modules/neovim
    ./modules/tmux-thumbs.nix
  ];
  home.packages =
    with pkgs;
    [
      nixpkgs-review
      nix-prefetch
      (pkgs.callPackage ./pkgs/atuin { })

      hexyl
      binutils
      ouch

      du-dust
      nixVersions.latest
      procs
      xcp

      socat
      tmux
      nurl
      htop
      hub
      tea
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
      nix-direnv
      fzf
      lsd
      zoxide
      pinentry-curses
      fd
      bat
      moar
      vivid
      ripgrep
      zsh
      less
      bashInteractive
      ncurses
      coreutils
      git

      self.packages.${pkgs.stdenv.hostPlatform.system}.mergify-gen-config
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      strace
      psmisc
      glibcLocales
      gdb
    ]
    ++ lib.optional (pkgs.hostPlatform.system != "riscv64-linux") nix-output-monitor;

  home.enableNixpkgsReleaseCheck = false;

  # better eval time
  manual.html.enable = false;
  manual.manpages.enable = false;
  manual.json.enable = false;

  home.stateVersion = "23.11";
  home.username = lib.mkDefault "joerg";
  home.homeDirectory =
    if pkgs.stdenv.isDarwin then "/Users/${config.home.username}" else "/home/${config.home.username}";
  programs.home-manager.enable = true;
}
