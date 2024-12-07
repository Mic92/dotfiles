{ pkgs, lib, ... }:
{

  clanCore.machineDescription = "Waypipe Demo";

  clan.virtualisation.memorySize = 8000;
  clan.virtualisation.cores = 4;
  #clan.virtualisation.waypipe.enable = true;

  #clan.services.waypipe.enable = true;
  #clan.services.waypipe.command = [ (lib.getExe pkgs.foot) ];

  #hardware.opengl.enable = true;
}
