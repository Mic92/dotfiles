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
        lib.makeBinPath [
          config.nix.package
          pkgs.curl
          pkgs.iproute2
          pkgs.nettools
          pkgs.gitMinimal
          pkgs.openssh
          pkgs.util-linux
        ]
      }
      # skip service if do not have a default route
      if ! ip r g 8.8.8.8; then
        exit
      fi
      store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/Mic92/dotfiles/main/${pkgs.stdenv.hostPlatform.system}.nixos-${config.networking.hostName})"
      nix-store --add-root /run/next-system -r "$store_path"

      if [[ -f /home/joerg/.homesick/repos/dotfiles/flake.nix ]]; then
        profile=$(runuser -u joerg -- nix run "/home/joerg/.homesick/repos/dotfiles/#hm" -- profile)
        store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/Mic92/dotfiles/main/${pkgs.stdenv.hostPlatform.system}.home-manager-$profile)"
        nix-store --add-root /run/next-home -r "$store_path"
      fi
    '';
    serviceConfig = {
      CPUSchedulingPolicy = "idle";
      IOSchedulingClass = "idle";
    };
  };
}
