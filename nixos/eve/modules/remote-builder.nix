{ config, ... }: {
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 96;
      sshKey = config.sops.secrets.ssh-aarch64-builder.path;
      protocol = "ssh-ng";
      sshUser = "mic92";
      system = "aarch64-linux";
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "mac01.numtide.com";
      sshUser = "hetzner";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-aarch64-builder.path;
      system = "aarch64-darwin";
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
      ];
    }
    # direct connection sometimes break, too many connections?
    {
      hostName = "amy.dse.in.tum.de";
      maxJobs = 92;
      sshKey = config.sops.secrets.tum-builder.path;
      protocol = "ssh-ng";
      sshUser = "nix";
      systems = [ "x86_64-linux" "i686-linux" ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "clara.dse.in.tum.de";
      maxJobs = 92;
      sshKey = config.sops.secrets.tum-builder.path;
      protocol = "ssh-ng";
      sshUser = "nix";
      systems = [ "x86_64-linux" "i686-linux" ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "rose.dse.in.tum.de";
      maxJobs = 92;
      sshKey = config.sops.secrets.tum-builder.path;
      protocol = "ssh-ng";
      sshUser = "nix";
      systems = [ "x86_64-linux" "i686-linux" ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
  ];
  programs.ssh.knownHosts = {
    "aarch64.nixos.community" = {
      hostNames = [ "aarch64.nixos.community" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMUTz5i9u5H2FHNAmZJyoJfIGyUm/HfGhfwnc142L3ds";
    };
    "mac01.numtide.com" = {
      hostNames = [ "mac01.numtide.com" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDrUaBPZktk41hYHN95IIHBL4bn7LbZy1aLra/ONv4go";
    };
  };
  sops.secrets.tum-builder = { };
  sops.secrets.ssh-aarch64-builder = { };
}
