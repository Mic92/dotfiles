{ config, lib, pkgs, ... }:
with builtins;

let
  backup_path = "/mnt/backup/borg";
  borg = pkgs.borgbackup.overrideDerivation (old: rec {
    version = "1.1.0rc3";
    src = pkgs.fetchurl {
      url = "https://github.com/borgbackup/borg/releases/download/${version}/borgbackup-${version}.tar.gz";
      sha256 = "076v9x3gqgnscfk1zrkp7290liz15jadz8rvcinmk6iknjn7hhjh";
    };
    postInstall = ""; # skip doc generation
  });
in {
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "12:00:00";
  };
  systemd.services.backup = {
    path = with pkgs; [ borg ];
    # cifs mount from ./dice.nix
    unitConfig.RequiresMountsFor = backup_path;
    script = ''
       export BORG_PASSPHRASE=$(cat /home/joerg/git/nixos-configuration/secrets/borgbackup)

       borg create --stats \
            --compression zlib,9 \
            "${backup_path}::turingmachine-$(date +%Y%m%d)" \
            /home /var /root /etc

       borg prune -v "${backup_path}" \
         --keep-daily 7 \
         --keep-weekly 4 \
         --keep-monthly 3

       # backup itself
       cp "$0" "${backup_path}/../backup-script"
    '';
  };
  environment.systemPackages = with pkgs; [ borg ];
}
