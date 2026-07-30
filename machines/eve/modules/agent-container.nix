# Shared defaults for LLM agent containers, imported inside the container
# config (containers.<name>.config.imports).
{ pkgs, ... }:
{
  environment.etc."timezone".text = "Europe/Berlin\n";

  environment.systemPackages = with pkgs; [
    bash
    bc
    cacert
    coreutils
    curl
    diffutils
    fd
    file
    findutils
    git
    gnugrep
    gnused
    gnutar
    gzip
    htmlq
    hurl
    jq
    less
    libarchive
    nix
    openssh
    patch
    procps
    python3
    ripgrep
    tree
    unzip
    util-linux
    w3m
    wget
    which
    xz
    yq-go
    zip
    zstd
  ];

  # nixos-containers bind-mount the host's /nix/store and daemon socket,
  # so nix commands work against the host daemon.
  nix = {
    enable = true;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    # Pin nixpkgs to the store path this system was built from, like the
    # host does, so `nix run nixpkgs#...` needs no registry download.
    registry.nixpkgs.to = {
      type = "path";
      path = pkgs.path;
    };
    nixPath = [ "nixpkgs=flake:nixpkgs" ];
  };
  environment.variables.NIX_REMOTE = "daemon";
}
