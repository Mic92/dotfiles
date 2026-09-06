{
  pkgs,
  config,
  lib,
  ...
}:
let
  maxUploadSize = "256M";
  toKeyValue = lib.generators.toKeyValue { mkKeyValue = lib.generators.mkKeyValueDefault { } " = "; };
in
{
  services.phpfpm.pools.snappymail = {
    user = "snappymail";
    group = "snappymail";
    phpOptions = toKeyValue {
      upload_max_filesize = maxUploadSize;
      post_max_size = maxUploadSize;
      memory_limit = maxUploadSize;
      # MailSo\Log\Logger installs a pcntl handler for SIGQUIT that
      # persists in the FPM worker; the master's idle-reap SIGQUIT then
      # hits zend_signal_handler in accept() and the worker dumps core
      # (~1500 coredumps/day with pm=ondemand).
      disable_functions = "pcntl_signal,pcntl_async_signals";
    };

    settings = {
      "listen.owner" = "nginx";
      "listen.group" = "nginx";
      "pm" = "ondemand";
      "pm.max_children" = 32;
      "pm.process_idle_timeout" = "10s";
      "pm.max_requests" = 500;
    };
  };

  services.postgresql.ensureDatabases = [ "snappymail" ];
  services.postgresql.ensureUsers = [
    {
      name = "snappymail";
      ensureDBOwnership = true;
    }
  ];

  services.nginx = {
    virtualHosts."mail.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        index index.php;
      '';
      locations."^~ /data".extraConfig = ''
        deny all;
      '';
      locations."~ \.php$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param   SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_pass unix:${config.services.phpfpm.pools.snappymail.socket};
      '';
      extraConfig = ''
        client_max_body_size ${maxUploadSize};
      '';
      root = pkgs.snappymail.override { dataPath = "/var/lib/snappymail"; };
    };
  };

  users.users.snappymail = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/snappymail";
    group = "snappymail";
  };

  users.groups.snappymail = { };
}
