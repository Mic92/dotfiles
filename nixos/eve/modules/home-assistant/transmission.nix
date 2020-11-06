{ config, lib, pkgs, ... }:

{
  services.home-assistant.config.transmission = {
    host = "yellow.r";
  };
}
