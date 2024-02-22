{
  services.borgbackup.repos = {
    eve = {
      path = "/zdata/borg/eve";
      authorizedKeys = [
        (builtins.readFile ../../../../machines/eve/facts/borgbackup.ssh.pub)
      ];
    };

    matchbox = {
      path = "/zdata/borg/matchbox";
      authorizedKeys = [
        # TODO: migrate to clan-backup
        (builtins.readFile ./matchbox-borgbackup.pub)
      ];
    };

    turingmachine = {
      path = "/zdata/borg/turingmachine";
      authorizedKeys = [
        (builtins.readFile ../../../../machines/turingmachine/facts/borgbackup.ssh.pub)
      ];
    };
  };
}
