{ pkgs, config, ... }:
{
  services.buildkite-agents.builder = {
    enable = true;
    tokenPath = config.sops.secrets.buildkite-token.path;
    privateSshKeyPath = config.sops.secrets.buildkite-ssh-key.path;

    runtimePackages = [
      pkgs.gnutar
      pkgs.bash
      pkgs.nix
      pkgs.gzip
      pkgs.git
    ];
  };

  systemd.services.buildkite-agent-builder = {
    confinement.enable = true;
    confinement.packages = config.services.buildkite-agents.builder.runtimePackages;
    serviceConfig = {
      SupplementaryGroups = [ "keys" ];
      BindReadOnlyPaths = [
        config.services.buildkite-agents.builder.tokenPath
        config.services.buildkite-agents.builder.privateSshKeyPath
        "${config.environment.etc."ssl/certs/ca-certificates.crt".source}:/etc/ssl/certs/ca-certificates.crt"
        "/etc/machine-id"
        # channels are dynamic paths in the nix store, therefore we need to bind mount the whole thing
        "/nix/store"
      ];
      BindPaths = [
        config.services.buildkite-agents.builder.dataDir
        "/nix/var/nix/daemon-socket/socket"
      ];
    };
  };

  sops.secrets.buildkite-token.owner = config.systemd.services.buildkite-agent-builder.serviceConfig.User;
  sops.secrets.buildkite-ssh-key.owner = config.systemd.services.buildkite-agent-builder.serviceConfig.User;
}
