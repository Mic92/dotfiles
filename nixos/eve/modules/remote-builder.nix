{
  config,
  lib,
  pkgs,
  ...
}: {
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 96;
      sshKey = config.sops.secrets.ssh-aarch64-builder.path;
      sshUser = "ssh-ng://mic92";
      system = "aarch64-linux";
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "ryan.dse.in.tum.de";
      maxJobs = 128;
      sshKey = config.sops.secrets.tum-builder.path;
      sshUser = "ssh-ng://nix";
      systems = ["x86_64-linux" "i686-linux"];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
  ];

  programs.ssh.knownHosts = {
    "aarch64.nixos.community" = {
      hostNames = ["aarch64.nixos.community"];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMUTz5i9u5H2FHNAmZJyoJfIGyUm/HfGhfwnc142L3ds";
    };
  };
  sops.secrets.tum-builder = {};
  sops.secrets.ssh-aarch64-builder = {};
}
