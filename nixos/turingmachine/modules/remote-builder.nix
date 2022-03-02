{config, ...}: {
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "ryan.r";
      sshUser = "ssh-ng://nix";
      sshKey = config.sops.secrets.id_buildfarm.path;
      system = "x86_64-linux";
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    #{
    #  hostName = "graham.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 64;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "nardole.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 10;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "bill.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 10;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "sauron.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 10;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "yasmin.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "aarch64-linux";
    #  maxJobs = 224;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    ##{
    ##  hostName = "martha.r";
    ##  sshUser = "ssh-ng://nix";
    ##  sshKey = config.sops.secrets.id_buildfarm.path;
    ##  system = "x86_64-linux";
    ##  maxJobs = 8;
    ##  supportedFeatures = [
    ##    "big-parallel"
    ##    "kvm"
    ##    "nixos-test"
    ##  ];
    ##}
    #{
    #  hostName = "donna.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 8;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "amy.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 8;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "clara.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 8;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #  hostName = "rose.r";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 8;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    ## slow...
    ##{
    ##  hostName = "prism.r";
    ##  sshUser = "ssh-ng://mic92";
    ##  sshKey = "/root/.ssh/id_ed25519";
    ##  system = "x86_64-linux";
    ##  maxJobs = 4;
    ##  supportedFeatures = [
    ##    "big-parallel"
    ##    "kvm"
    ##    "nixos-test"
    ##  ];
    ##}
    #{
    #  hostName = "eve.thalheim.io";
    #  sshUser = "ssh-ng://nix";
    #  sshKey = config.sops.secrets.id_buildfarm.path;
    #  system = "x86_64-linux";
    #  maxJobs = 4;
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    #{
    #    # rpi3
    #    hostName = "matchbox.r";
    #    maxJobs = 4;
    #    sshKey = config.sops.secrets.id_buildfarm.path;
    #    sshUser = "nix";
    #    system = "aarch64-linux";
    #}
    {
      hostName = "aarch64.nixos.community";
      maxJobs = 96;
      sshKey = "/root/.ssh/id_ed25519";
      sshUser = "ssh-ng://mic92";
      system = "aarch64-linux";
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
  ];
  sops.secrets.id_buildfarm = {};
}
