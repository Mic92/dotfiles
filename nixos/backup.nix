{ config, lib, pkgs, ... }:
with builtins;

let
 backup_path = "/mnt/backup/borg";
in {
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "12:00:00";
  };
  systemd.services.backup = {
    path = with pkgs; [ borgbackup ];
    # cifs mount from ./dice.nix
    unitConfig.RequiresMountsFor = backup_path;
    script = ''
       export BORG_PASSPHRASE=$(cat /home/joerg/git/nixos-configuration/secrets/borgbackup)

       borg create --stats "${backup_path}::turingmachine-$(date +%Y%m%d)" \
            --compression zlib,9 \
            /home /var /root /etc

       borg prune -v "${backup_path}" \
         --keep-daily 7 \
         --keep-weekly 4 \
         --keep-monthly 3 \

       # backup itself
       cp "$0" "${backup_path}/../backup-script"
    '';
  };
  environment.systemPackages = with pkgs; [
    borgbackup
  ];
}
