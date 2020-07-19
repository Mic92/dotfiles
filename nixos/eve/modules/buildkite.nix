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

  systemd.services."buildkite-agent-builder" = {
    serviceConfig.SupplementaryGroups = [ "keys" ];
  };

  sops.secrets.buildkite-token.owner = "buildkite-agent-builder";
  sops.secrets.buildkite-ssh-key.owner = "buildkite-agent-builder";
}
