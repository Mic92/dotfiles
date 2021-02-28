{ config, pkgs, ... }: {
  users.extraUsers.borgbackup = {
    isSystemUser = true;
    createHome = true;
    uid = 999;
    shell = "/run/current-system/sw/bin/bash";
    home = "/var/lib/borgbackup";
    openssh.authorizedKeys.keys =
      let
        borgCmd = path: key:
          ''command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /var/lib/borgbackup/${path}",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${key}'';
      in
      [
        (borgCmd "rose" (builtins.readFile ./rose-borgbackup.pub))
        (borgCmd "turingmachine/borg" (builtins.readFile ./turingmachine-borgbackup.pub))
      ];
  };

  fileSystems."/var/lib/borgbackup/turingmachine" = {
    device = "//csce.datastore.ed.ac.uk/csce/inf/users/s1691654";
    fsType = "cifs";
    options =
      let
        automount_opts = "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s,soft,vers=2.0,uid=${toString config.users.extraUsers.borgbackup.uid}";
        # cat > smb-secrets <<EOF
        # username=s16916XX
        # domain=ED
        # password=<EASE_PASSWORD>
        # EOF
      in
      [ "${automount_opts},credentials=${config.sops.secrets.smb-secrets.path}" ];
  };
  sops.secrets.smb-secrets = { };
}
