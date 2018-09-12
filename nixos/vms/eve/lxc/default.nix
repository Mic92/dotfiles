with import <nixpkgs> {};

pkgs.bundlerEnv {
  name = "lxc-ruby";
  inherit (pkgs) ruby;
  gemfile = ./Gemfile;
  lockfile = ./Gemfile.lock;
  gemset = ./gemset.nix;
}
