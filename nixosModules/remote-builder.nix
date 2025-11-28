{ config, ... }:
{
  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      hostName = "jamie";
      sshUser = "nix";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-remote-builder.path;
      systems = [
        "x86_64-linux"
        "i686-linux"
      ];
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    {
      hostName = "eliza";
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
      hostName = "mac02.numtide.com";
      sshUser = "customer";
      protocol = "ssh-ng";
      sshKey = config.sops.secrets.ssh-remote-builder.path;
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      maxJobs = 8;
      supportedFeatures = [ "big-parallel" ];
    }
  ];

  programs.ssh.extraConfig = ''
    Host mac02.numtide.com
      User customer
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host irene
      User nix
      ProxyJump login-tum
      HostName irene.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host jamie
      User nix
      ProxyJump login-tum
      HostName jamie.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host eliza
      User nix
      ProxyJump login-tum
      HostName eliza.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host login-tum
      User tunnel
      HostName login.dse.in.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
  '';
}
