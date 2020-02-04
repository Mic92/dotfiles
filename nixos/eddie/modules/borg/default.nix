{ pkgs, ... }: {
  users.extraUsers.borgbackup = {
    isSystemUser = true;
    createHome = true;
    shell = "/run/current-system/sw/bin/bash";
    home = "/var/lib/borgbackup";
    openssh.authorizedKeys.keys = [
      ''command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /var/lib/borgbackup/rose",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${builtins.readFile ./rose-borgbackup.pub}''
    ];
  };
}
