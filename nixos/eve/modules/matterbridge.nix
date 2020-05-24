{ config, ... }: {
  services.matterbridge.enable = true;
  services.matterbridge.configPath = config.krops.secrets."matterbridge.toml".path;
  krops.secrets."matterbridge.toml".owner = config.services.matterbridge.user;

  systemd.services.matterbridge.serviceConfig.SupplementaryGroups = [ "keys" ];
  users.users.matterbridge.extraGroups = [ "keys" ];
}
