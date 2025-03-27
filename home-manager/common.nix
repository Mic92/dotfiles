{
  pkgs,
  config,
  lib,
  self,
  inputs,
  ...
}:
{
  imports = [
    ./modules/neovim
    ./modules/tmux-thumbs.nix
  ];

  nix.package = inputs.nix.packages.${pkgs.hostPlatform.system}.nix;
  #nix.package = pkgs.nixVersions.latest;

  home.packages =
    with pkgs;
    [
      config.nix.package
      nixpkgs-review
      nix-prefetch
      (pkgs.callPackage ./pkgs/atuin { })
      mergiraf

      hexyl
      binutils
      ouch

      du-dust

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
      gnused
      gnugrep
      findutils
      ncurses
      coreutils
      git

      self.packages.${pkgs.stdenv.hostPlatform.system}.mergify-gen-config
      self.packages.${pkgs.stdenv.hostPlatform.system}.merge-when-green
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      strace
      psmisc
      glibcLocales
      gdb
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [ iproute2mac ]
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
