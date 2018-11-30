{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    nix-prefetch-scripts
    pypi2nix
    go2nix
    mercurial # go2nix
    nox
    #(pkgs.callPackage /home/joerg/git/nix-review {})
    nur.repos.mic92.nix-review-unstable

    gdb
    strace
    binutils

    # see config.nix
    myvim
    # python language server + plugins
    (python3.withPackages(ps: [ ps.pyls-mypy ps.pyls-isort ps.pyls-black ]))

    tmux
    htop
    psmisc
    expect
    gitAndTools.hub
    gitAndTools.tig
    jq
    httpie
    cloc
    mosh
    cheat
    tldr
    gnupg1compat
    direnv
    fzf
    exa
    fd
    bat
  ];

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
}
