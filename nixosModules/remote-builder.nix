{ config, ... }:
{
  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      hostName = "graham";
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
      hostName = "yasmin";
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
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      maxJobs = 8;
      supportedFeatures = [ "big-parallel" ];
    }
  ];

  programs.ssh.extraConfig = ''
    Host irene
      User nix
      ProxyJump login-tum
      HostName irene.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host graham
      User nix
      ProxyJump login-tum
      HostName graham.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host yasmin
      User nix
      ProxyJump login-tum
      HostName yasmin.dos.cit.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host login-tum
      User tunnel
      HostName login.dse.in.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
  '';
}
