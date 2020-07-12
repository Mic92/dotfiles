{ pkgs, config, ... }:
let
  secrets = config.krops.secrets;
in {
  services.buildkite-agents.builder = {
    enable = true;
    tokenPath = secrets.buildkite-token.path;
    privateSshKeyPath =  secrets.buildkite-ssh-key.path;

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

  krops.secrets.buildkite-token.owner = "buildkite-agent-builder";
  krops.secrets.buildkite-ssh-key.owner = "buildkite-agent-builder";
}
