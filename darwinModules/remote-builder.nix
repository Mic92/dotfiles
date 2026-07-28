{
  config,
  self,
  ...
}:
{
  # grpc:// store plugin so nix can reach eve's gRPC nix-daemon. The
  # dispatcher loads the plugin matching the running Nix version and is safe
  # to load globally via nix.conf. The flake only ships a NixOS module, but
  # client.nix only touches nix.settings, which nix-darwin provides too.
  imports = [ "${self.inputs.nix-grpc-store}/nixos/client.nix" ];
  programs.nix-grpc-store.enable = true;

  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      # gRPC nix-daemon on eve (client cert via `nix-grpc-cert`, see
      # nixosModules/nix-grpc-cert.nix; the plugin defaults to certs in
      # /run/nix-grpc-store and the system CA bundle); eve fans out to its
      # own builders.
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
    Host mac02.numtide.com
      User customer
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
    Host login-tum
      User tunnel
      HostName login.dse.in.tum.de
      IdentityFile ${config.sops.secrets.ssh-remote-builder.path}
  '';
}
