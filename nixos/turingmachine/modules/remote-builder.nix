{ config, ... }: {
  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      hostName = "vislor.dse.in.tum.de";
      sshUser = "nix";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-remote-builder.path;
      systems = [ "x86_64-linux" "i686-linux" ];
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "yasmin.dse.in.tum.de";
      sshUser = "nix";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-remote-builder.path;
      system = "aarch64-linux";
      maxJobs = 224;
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
      sshKey = "/root/.ssh/id_ed25519";
      systems = [ "aarch64-darwin" "x86_64-darwin" ];
      maxJobs = 8;
      supportedFeatures = [
        "big-parallel"
      ];
    }
    #{
    #  hostName = "aarch64.nixos.community";
    #  maxJobs = 96;
    #  sshKey = "/root/.ssh/id_ed25519";
    #  sshUser = "ssh-ng://mic92";
    #  system = "aarch64-linux";
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
  ];
}
