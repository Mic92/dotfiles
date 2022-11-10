{
  config,
  lib,
  pkgs,
  ...
}: {
  systemd.timers.update-prefetch.timerConfig.Persistent = "yes";
  systemd.services.update-prefetch = {
    startAt = "hourly";
    script = ''
      export PATH=${lib.makeBinPath (with pkgs; [config.nix.package pkgs.curl pkgs.iproute2 pkgs.nettools pkgs.git pkgs.openssh])}
      # skip service if do not have a default route
      if ! ip r g 8.8.8.8; then
        exit
      fi
      HOST=$(</proc/sys/kernel/hostname)
      store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/nixos-$HOST)"
      nix copy --from https://cache.thalheim.io "$store_path"
      nix-store --add-root /run/next-system -r "$store_path"

      if [[ -f /home/joerg/.homesick/repos/dotfiles/flake.nix ]]; then
        profile=$(nix run "/home/joerg/.homesick/repos/dotfiles/#hm" -- profile)
        store_path="$(curl -L https://buildbot.thalheim.io/nix-outputs/home-manager-$profile)"
        nix copy --from https://cache.thalheim.io "$store_path"
        nix-store --add-root /run/next-home -r "$store_path"
      fi
    '';
    serviceConfig = {
      CPUSchedulingPolicy = "idle";
      IOSchedulingClass = "idle";
    };
  };

  programs.ssh.extraConfig = ''
    Host binary-cache.thalheim.io
      User nix-ssh
      IdentityFile ${config.sops.secrets.binary-cache-key.path}
  '';
  sops.secrets.binary-cache-key.sopsFile = ../secrets/secrets.yaml;
}
