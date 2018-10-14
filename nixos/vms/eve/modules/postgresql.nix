{
  services.postgresql.enable = true;

  services.postgresqlBackup = {
    enable = true;
    databases = [ 
      "gogs" 
      "grafana"
      "mediawiki"
      "nextcloud" 
      "postgres"
      "prosody"
      "rainloop"
      "tt_rss" 
    ];
  };

  environment.etc."netdata/python.d/postgres.conf".text = ''
    socket:
      name     : 'local'
      user     : 'netdata'
      database : 'postgres'
  '';
}
