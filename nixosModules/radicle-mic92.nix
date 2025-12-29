# Personal Radicle configuration - seeds mic92's repositories across all nodes
{
  imports = [ ./radicle-node.nix ];

  services.radicle.seedRepositories = [
    "rad:z3gpeDzWxqV8iBEN8RcJNZEPVWmJf" # gif-searcher
    "rad:z2dqRKkK5yu89w3CMX2dVsYrRwvFk" # dotfiles
    "rad:z2T41Ec4Hy452KDm6g6BVvNR2G9Cw" # flake-fmt
    "rad:z3i92bZDrPYnftcX7rnVLxPgFJPdQ" # nix-fast-build
  ];
}
