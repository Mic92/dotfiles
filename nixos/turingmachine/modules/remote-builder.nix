{ ... }: {
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "martha.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 8;
  } {
    hostName = "donna.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 8;
  } {
    hostName = "amy.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 8;
  } {
    hostName = "clara.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 8;
  } {
    hostName = "rose.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 8;
  } {
    hostName = "prism.r";
    sshUser = "Mic92";
    sshKey = "/root/.ssh/id_ed25519";
    system = "x86_64-linux";
    maxJobs = 4;
  } {
    hostName = "eve.thalheim.io";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 4;
  } {
    hostName = "inspector.r";
    sshUser = "nix";
    sshKey = "/var/src/secrets/id_buildfarm";
    system = "x86_64-linux";
    maxJobs = 4;
  } {
    hostName = "dpdkm.r";
    sshKey = "/var/src/secrets/id_buildfarm";
    sshUser = "nix";
    system = "x86_64-linux";
    maxJobs = 4;
  } {
    #  hostName = "eddie.r";
    #  sshKey = "/var/src/secrets/id_buildfarm";
    #  sshUser = "nix";
    #  system = "x86_64-linux";
    #  maxJobs = 2;
    #} {
    # rpi3
    #  hostName = "172.23.75.254";
    #  maxJobs = 4;
    #  sshKey = "/var/src/secrets/id_buildfarm";
    #  sshUser = "nix";
    #  system = "aarch64-linux";
    #} {
    hostName = "aarch64.nixos.community";
    maxJobs = 96;
    sshKey = "/root/.ssh/id_ed25519";
    sshUser = "mic92";
    system = "aarch64-linux";
    supportedFeatures = [ "big-parallel" ];
  }
                      ];
}
