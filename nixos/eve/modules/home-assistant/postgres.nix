{
  services.home-assistant.config = {
    recorder.db_url = "postgresql://@/hass";
  };
  services.postgresql = {
    ensureDatabases = [ "hass" ];
    ensureUsers = [
      {
        name = "hass";
        ensurePermissions = {
          "DATABASE hass" = "ALL PRIVILEGES";
        };
      }
    ];
  };
}
