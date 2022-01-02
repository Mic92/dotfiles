{ lib, config, pkgs, inputs, ... }: with lib; {
  nix = {
    trustedUsers = [ "joerg" "root" ];
    gc.automatic = true;
    gc.dates = "03:15";
    # should be enough?
    nrBuildUsers = lib.mkDefault 32;

    daemonIOSchedClass = "idle";
    daemonCPUSchedPolicy = "idle";

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      builders-use-substitutes = true
      keep-outputs = true
      keep-derivations = true
      # in zfs we trust
      fsync-metadata = ${lib.boolToString (!config.boot.isContainer or config.fileSystems."/".fsType != "zfs")}
      experimental-features = nix-command flakes
    '';

    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://mic92.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
    ];
  };

  imports = [ ./builder.nix ];

  programs.command-not-found.enable = false;

  systemd.services.update-prefetch = {
    startAt = "hourly";
    script = ''
      export PATH=${lib.makeBinPath (with pkgs; [ config.nix.package pkgs.nettools pkgs.git pkgs.jq pkgs.curl ])}
      last_build=$(curl https://api.github.com/repos/Mic92/dotfiles/git/ref/tags/last-build | jq -r .object.sha)
      nix build \
       --out-link /run/next-system \
       github:Mic92/dotfiles/$last_build#nixosConfigurations.$(hostname).config.system.build.toplevel

      if [[ -x /home/joerg/.nix-profile/bin/home-manager ]]; then
        nix run github:Mic92/dotfiles/$last_build#hm-build --  --out-link /run/next-home
      fi
    '';
  };

  # inputs == flake inputs in configurations.nix
  environment.etc = let
    inputsWithDate = lib.filterAttrs (_: input: input ? lastModified) inputs;
    flakeAttrs = input: (lib.mapAttrsToList (n: v: ''${n}="${v}"'')
      (lib.filterAttrs (n: v: (builtins.typeOf v) == "string") input));
    lastModified = name: input: ''
      flake_input_last_modified{input="${name}",${lib.concatStringsSep "," (flakeAttrs input)}} ${toString input.lastModified}'';
  in {
    "flake-inputs.prom" = {
      mode = "0555";
      text = ''
        # HELP flake_registry_last_modified Last modification date of flake input in unixtime
        # TYPE flake_input_last_modified gauge
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList lastModified inputsWithDate)}
      '';
    };
  };

  services.telegraf.extraConfig.inputs.file = [{
    data_format = "prometheus";
    files = [ "/etc/flake-inputs.prom" ];
  }];

  nixpkgs.config.allowUnfree = true;
}
