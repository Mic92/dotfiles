{
  config,
  self,
  ...
}:
{
  # grpc:// store plugin so nix can reach eve's gRPC nix-daemon. The
  # dispatcher loads the plugin matching the running Nix version and is safe
  # to load globally via nix.conf.
  imports = [ self.inputs.nix-grpc-store.nixosModules.client ];
  programs.nix-grpc-store.enable = true;

  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      # gRPC nix-daemon on eve; client cert via `nix-grpc-cert`.
      hostName = "grpc://eve.thalheim.io:50051";
      protocol = null;
      systems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
      ];
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
        "recursive-nix"
        "uid-range"
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
      supportedFeatures = [
        "big-parallel"
        "recursive-nix"
      ];
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
      # The jumphost only has a cluster-CA signed certificate on its ecdsa/rsa
      # host keys; the ed25519 key carries an ITO-signed certificate we do not
      # trust. Host blocks match the alias, so repeat the algorithm preference.
      HostKeyAlgorithms ecdsa-sha2-nistp256-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com
  '';
}
