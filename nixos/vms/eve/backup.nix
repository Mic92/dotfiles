{ config, lib, pkgs, ... }:

with builtins;

{
  systemd.timers.backup = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnCalendar = "02:07:00";
  };
  systemd.services.backup = {
    path = with pkgs; [borgbackup openssh lxc bash gzip bzip2];
    script = ''
      set -eu -o pipefail
      cat '/etc/nixos/secrets/nas-wakeup-password' | ${pkgs.netcat}/bin/nc -w1 -v 172.23.75.65 22198
      ${pkgs.ruby}/bin/ruby /etc/nixos/scripts/backup-container
    '';
  };
}
