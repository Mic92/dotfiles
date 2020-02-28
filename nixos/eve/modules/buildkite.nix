{ pkgs, config, ... }:
let
  secrets = config.krops.secrets;
in {
  services.buildkite-agents.choose-place = {
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

  systemd.services."buildkite-agent-choose-place" = {
    serviceConfig.SupplementaryGroups = [ "keys" ];
  };

  krops.secrets.buildkite-token.owner = "buildkite-agent-choose-place";
  krops.secrets.buildkite-ssh-key.owner = "buildkite-agent-choose-place";
}
