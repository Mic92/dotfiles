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
  launchd.daemons.nix-daemon.serviceConfig.EnvironmentVariables.NIX_CONFIG = ''
    plugin-files = ${nix-grpc-store}/lib/nix/plugins
  '';

  nix.distributedBuilds = true;

  nix.buildMachines = [
    {
      # gRPC nix-daemon on eve (client cert via `nix-grpc-cert`, see
      # nixosModules/nix-grpc-cert.nix); eve fans out to its own builders.
      hostName = "grpc://eve.thalheim.io:50051?client-cert=/run/nix-grpc-store/client.crt&client-key=/run/nix-grpc-store/client.key&ca-cert=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
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
