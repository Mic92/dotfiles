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
}
