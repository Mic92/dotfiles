{ pkgs, ... }:
{
  imports = [ ../../../nixosModules/hass-agent.nix ];
  users.users.hass-agent.packages = [ pkgs.mosquitto ];
}
