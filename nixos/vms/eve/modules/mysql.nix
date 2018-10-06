{ pkgs, config, ... }: {
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = [
      "istwiki"
      "mysqlbackup"
    ];
    ensureUsers = [{
        ensurePermissions = { 
          "istwiki.*" = "ALL PRIVILEGES";
        };
        name = "istwiki";
      }];
    };

  services.mysqlBackup = {
    enable = true;
    databases = config.services.mysql.ensureDatabases;
  };

  deployment.keys = {
    "mysql-passwords.sql".keyFile = ../secrets/mysql-passwords.sql;
  };

  systemd.services.mysql.postStart = let
    script = pkgs.writeTextFile {
      name = "init.sql";
      text = ''
        CREATE USER IF NOT EXISTS 'istwiki'@'%';
        GRANT ALL PRIVILEGES ON istwiki.* TO 'istwiki'@'%';
      '';
    };
  in ''
    cat ${script} /run/keys/mysql-passwords.sql | ${config.services.mysql.package}/bin/mysql -u root -N || true
  '';
}
