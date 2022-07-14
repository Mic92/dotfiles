{ config, lib, pkgs, ... }:
{
  systemd.services.update-prefetch = {
    startAt = "hourly";
    script = ''
      export PATH=${lib.makeBinPath (with pkgs; [config.nix.package pkgs.curl pkgs.iproute2 pkgs.nettools pkgs.git])}
      # skip service if do not have a default route
      if ! ip r g 8.8.8.8; then
        exit
      fi
      HOST=$(</proc/sys/kernel/hostname)
      nix-store --add-root /run/next-system -r "$(curl -L buildbot.thalheim.io/nix-outputs/nixos-$HOST)"

      if [[ -f /home/joerg/.homesick/repos/dotfiles/flake.nix ]]; then
        profile=$(nix run "/home/joerg/.homesick/repos/dotfiles/#hm" -- profile)
        nix-store --add-root /run/next-home -r "$(curl -L buildbot.thalheim.io/nix-outputs/home-manager-$profile)"
      fi
    '';
    serviceConfig = {
      CPUSchedulingPolicy = "idle";
      IOSchedulingClass = "idle";
    };
  };
}
