{ config, lib, pkgs, ... }:
{
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "aarch64.nixos.community";
    maxJobs = 96;
    sshKey = config.sops.secrets.ssh-aarch64-builder.path;
    sshUser = "mic92";
    system = "aarch64-linux";
    supportedFeatures = [
      "big-parallel"
      "kvm"
      "nixos-tests"
    ];
  } {
    hostName = "localhost";
    maxJobs = 8;
    sshKey = config.sops.secrets.ssh-aarch64-builder.path;
    sshUser = "root";
    system = "x86_64-linux";
    supportedFeatures = [
      "big-parallel"
      "kvm"
      "nixos-tests"
    ];
  }];

  sops.secrets.ssh-aarch64-builder = {};
}
