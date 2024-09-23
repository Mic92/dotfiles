{
  config,
  lib,
  pkgs,
  ...
}:
{
  systemd.timers.update-prefetch.timerConfig.Persistent = "yes";
  systemd.services.update-prefetch = {
    startAt = "hourly";
    script = ''
      set -eux -o pipefail
      export PATH=${
        lib.makeBinPath ([
          config.nix.package
          pkgs.curl
          pkgs.iproute2
          pkgs.nettools
          pkgs.git
          pkgs.openssh
        ])
      }
      # skip service if do not have a default route
      if ! ip r g 8.8.8.8; then
        exit
      fi
      HOST=$(</proc/sys/kernel/hostname)
      store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/Mic92/dotfiles/main/${pkgs.hostPlatform.system}.nixos-$HOST)"
      nix-store --add-root /run/next-system -r "$store_path"

      if [[ -f /home/joerg/.homesick/repos/dotfiles/flake.nix ]]; then
        profile=$(nix run "/home/joerg/.homesick/repos/dotfiles/#hm" -- profile)
        store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/Mic92/dotfiles/main/${pkgs.hostPlatform.system}.home-manager-$profile)"
        nix-store --add-root /run/next-home -r "$store_path"
      fi
    '';
    serviceConfig = {
      CPUSchedulingPolicy = "idle";
      IOSchedulingClass = "idle";
    };
  };
}
