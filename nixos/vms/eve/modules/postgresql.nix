{
	services.postgresql.enable = true;

	services.postgresqlBackup = {
    enable = true;
		databases = [
			"postgres" "prosody" "grafana" "ttrss" "gogs" "rainloop" "owncloud"
		];
	};
}
