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
    ./modules/homeshick.nix
  ];

  nix.package = self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
  #nix.package = pkgs.nixVersions.latest;

  home.packages =
    with pkgs;
    [
      config.nix.package
      nixpkgs-review
      nix-prefetch
      mergiraf
      jujutsu

      hexyl
      binutils
      ouch

      dust

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
      eza
      zoxide
      pinentry-curses
      fd
      bat
      moor
      vivid
      ripgrep
      ast-grep
      zsh
      less
      bashInteractive
      gnused
      gnugrep
      findutils
      ncurses
      glow
      (pkgs.runCommand "uutils-coreutils" { } ''
        mkdir -p $out/bin
        for i in ${pkgs.uutils-coreutils}/bin/*; do
          ln -s "$i" "$out/bin/$(basename "''${i#${pkgs.uutils-coreutils}/bin/uutils-}")"
        done
      '')
      git
      radare2
      mypy

      self.packages.${pkgs.stdenv.hostPlatform.system}.merge-when-green
      self.packages.${pkgs.stdenv.hostPlatform.system}.buildbot-pr-check
      self.inputs.direnv-instant.packages.${pkgs.stdenv.hostPlatform.system}.default
      self.inputs.flake-fmt.packages.${pkgs.stdenv.hostPlatform.system}.default
      self.inputs.nix-diff-rs.packages.${pkgs.stdenv.hostPlatform.system}.default
      self.inputs.nix-tree-rs.packages.${pkgs.stdenv.hostPlatform.system}.default
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      strace
      psmisc
      glibcLocales
      gdb
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [ iproute2mac ]
    ++ lib.optional (pkgs.stdenv.hostPlatform.system != "riscv64-linux") nix-output-monitor;

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
