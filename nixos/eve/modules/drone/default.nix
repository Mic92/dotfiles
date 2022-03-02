{
  pkgs,
  config,
  ...
}: {
  imports = [
    ./server.nix
    ./exec-runner.nix
    ./ssh-runner.nix
  ];
  sops.secrets.drone = {};
}
