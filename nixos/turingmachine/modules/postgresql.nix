{ ... }: {
  services.postgresql.enable = true;
  services.postgresql.ensureDatabases = [ "joerg" ];
  services.postgresql.ensureUsers = [{
    name = "joerg";
    ensureDBOwnership = true;
  }];
}
