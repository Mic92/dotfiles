{ pkgs
, config
, ...
}: {
  systemd.services.tts = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.espeak ];
    environment.HOME = "/var/lib/tts";
    serviceConfig = {
      StateDirectory = "tts";
      DynamicUser = true;
      User = "tts";
      Group = "tts";
      ExecStart = ''
        ${pkgs.tts}/bin/tts-server --model_name tts_models/en/ljspeech/vits--neon --port 5004
      '';
    };
  };

  # TODO
  security.acme.certs."tts.r".server = config.retiolum.ca.acmeURL;

  services.nginx = {
    upstreams = {
      "@tts".extraConfig = "server localhost:5004;";
    };
    virtualHosts."tts.r" = {
      # TODO
      enableACME = true;
      addSSL = true;
      locations."/".extraConfig = ''
        proxy_pass       http://@tts/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Host $host:80;
        proxy_set_header X-Forwarded-Server $host;
        proxy_set_header X-Forwarded-Port 80;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
    };
  };
}
