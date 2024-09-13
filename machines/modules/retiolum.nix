{ config, ... }:
{
  services.tinc.networks.retiolum = {
    ed25519PrivateKeyFile = config.sops.secrets."${config.clan.core.machineName}-tinc-ed25519".path;
    rsaPrivateKeyFile = config.sops.secrets."${config.clan.core.machineName}-tinc-rsa".path;
  };

  # only allow connections from hosts specified in our retiolum hosts.
  services.tinc.networks.retiolum.extraConfig = "StrictSubnets yes";
}
