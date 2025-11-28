{ config, ... }:
{
  nix.distributedBuilds = true;
  nix.buildMachines = [
    #{
    #  hostName = "aarch64.nixos.community";
    #  maxJobs = 96;
    #  sshKey = config.sops.secrets.ssh-aarch64-builder.path;
    #  protocol = "ssh-ng";
    #  sshUser = "mic92";
    #  system = "aarch64-linux";
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
    {
      hostName = "mac02";
      sshUser = "customer";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-tum-builder.path;
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      maxJobs = 8;
      supportedFeatures = [ "big-parallel" ];
    }
    # direct connection sometimes break, too many connections?
    {
      hostName = "jamie";
      maxJobs = 128;
      sshKey = config.sops.secrets.ssh-tum-builder.path;
      protocol = "ssh-ng";
      sshUser = "nix";
      systems = [
        "x86_64-linux"
        "i686-linux"
      ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "eliza";
      maxJobs = 128;
      sshKey = config.sops.secrets.ssh-tum-builder.path;
      protocol = "ssh-ng";
      sshUser = "nix";
      systems = [ "aarch64-linux" ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
  ];
  programs.ssh.extraConfig = ''
    Host mac02
      User customer
      HostName mac02.numtide.com
      IdentityFile ${config.sops.secrets.ssh-tum-builder.path}
    Host jamie
      User nix
      ProxyJump login-tum
      HostName jamie.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-tum-builder.path}
    Host eliza
      User nix
      ProxyJump login-tum
      HostName eliza.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-tum-builder.path}
    Host login-tum
      User tunnel
      HostName login.dse.in.tum.de
      IdentityFile ${config.sops.secrets.ssh-tum-builder.path}
  '';
  programs.ssh.knownHosts = {
    "aarch64.nixos.community" = {
      hostNames = [ "aarch64.nixos.community" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMUTz5i9u5H2FHNAmZJyoJfIGyUm/HfGhfwnc142L3ds";
    };
    "mac02.numtide.com" = {
      hostNames = [ "mac02.numtide.com" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGX2wsoPj5j08Uuzt0AF5gA6lPiZS6fU3gKSf9XMcoXd";
    };
  };
}
