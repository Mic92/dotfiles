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
    #{
    #  hostName = "aarch64.nixos.community";
    #  maxJobs = 96;
    #  sshKey = "/root/.ssh/id_ed25519";
    #  protocol = "ssh-ng";
    #  sshUser = "mic92";
    #  system = "aarch64-linux";
    #  supportedFeatures = [
    #    "big-parallel"
    #    "kvm"
    #    "nixos-test"
    #  ];
    #}
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
