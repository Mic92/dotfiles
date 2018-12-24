{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    nix-prefetch-scripts
    #(pkgs.callPackage /home/joerg/git/nix-review {})
    nur.repos.mic92.nix-review-unstable

    gdb
    strace
    binutils

    # see config.nix
    myvim
    emacs
    # python language server + plugins
    (python3.withPackages(ps: [
      ps.pyls-mypy
      ps.pyls-isort
      ps.pyls-black
      ps.flake8
      ps.mypy
    ]))

    tmux
    htop
    psmisc
    gitAndTools.hub
    gitAndTools.tig
    cloc
    mosh
    direnv
    fzf
    exa
    fd
    bat
    vivid
    silver-searcher
  ];

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
  home.stateVersion = "18.09";
}
