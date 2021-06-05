{ config, ... }:
{
  sops.secrets.hercules-join-token.owner = "hercules-ci-agent";
  sops.secrets.hercules-binary-cache.owner = "hercules-ci-agent";

  services.hercules-ci-agent = {
    enable = true;
    settings = {
      clusterJoinTokenPath = config.sops.secrets.hercules-join-token.path;
      binaryCachesPath = config.sops.secrets.hercules-binary-cache.path;
    };
  };
}
