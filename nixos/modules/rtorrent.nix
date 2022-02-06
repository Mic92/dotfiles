{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.rtorrent
  ];
  networking.firewall.allowedTCPPortRanges =  [
    # rtorrent
    { from = 60000; to = 60010; }
  ];
}
