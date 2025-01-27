{ pkgs, ... }:
{
  # iptables muscle memory
  environment.systemPackages = [ pkgs.iptables ];
  networking.nftables.enable = true;
}
