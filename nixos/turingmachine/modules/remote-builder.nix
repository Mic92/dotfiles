{ config, ... }: {
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "martha.r";
    sshUser = "nix";
    sshKey = config.sops.secrets.id_buildfarm.path;
    system = "x86_64-linux";
    maxJobs = 8;
    supportedFeatures = [
      "big-parallel"
      "kvm"
      "nixos-test"
    ];
  }
    {
      hostName = "donna.r";
      sshUser = "nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "amy.r";
      sshUser = "nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "clara.r";
      sshUser = "nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "rose.r";
      sshUser = "nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "prism.r";
      sshUser = "mic92";
      sshKey = "/root/.ssh/id_ed25519";
      system = "x86_64-linux";
      maxJobs = 4;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "eve.thalheim.io";
      sshUser = "nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 4;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
      #} {
      #  hostName = "eddie.r";
      #  sshKey = config.sops.secrets.id_buildfarm.path;
      #  sshUser = "nix";
      #  system = "x86_64-linux";
      #  maxJobs = 2;
    }
    {
      # rpi3
      #  hostName = "172.23.75.254";
      #  maxJobs = 4;
      #  sshKey = config.sops.secrets.id_buildfarm.path;
      #  sshUser = "nix";
      #  system = "aarch64-linux";
      #} {
      hostName = "aarch64.nixos.community";
      maxJobs = 96;
      sshKey = "/root/.ssh/id_ed25519";
      sshUser = "mic92";
      system = "aarch64-linux";
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }];
  sops.secrets.id_buildfarm = { };
}
