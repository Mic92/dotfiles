{
  services.borgbackup.repos = {
    eve = {
      path = "/zdata/borg/eve";
      authorizedKeys = [
        (builtins.readFile ./eve-borgbackup.pub)
      ];
    };

    matchbox = {
      path = "/zdata/borg/matchbox";
      authorizedKeys = [
        (builtins.readFile ./matchbox-borgbackup.pub)
      ];
    };

    turingmachine = {
      path = "/zdata/borg/turingmachine";
      authorizedKeys = [
        (builtins.readFile ./turingmachine-borgbackup.pub)
      ];
    };
  };
}
