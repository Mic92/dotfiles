{
  config,
  self,
  pkgs,
  ...
}:
let
  # grpc:// store plugin for the nix-daemon so it can reach eve's gRPC
  # nix-daemon. Loaded only in the daemon (not global nix.conf): other nix
  # binaries on the system (e.g. home-manager's stable nix) have a different
  # C++ ABI and crash when dlopen()ing the plugin.
  nix-grpc-store = pkgs.callPackage "${self.inputs.nix-grpc-store}/package.nix" {
    inherit (config.nix.package.libs) nix-store nix-util;
  };
in
{
  systemd.services.nix-daemon.environment.NIX_CONFIG = ''
    plugin-files = ${nix-grpc-store}/lib/nix/plugins
  '';

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
