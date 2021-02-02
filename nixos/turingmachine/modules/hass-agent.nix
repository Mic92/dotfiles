{ pkgs, ... }: {
  imports = [ ../../modules/hass-agent.nix ];
  users.users.hass-agent.packages = [ pkgs.mosquitto ];
}
