{ pkgs, ... }: {
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
    12101
  ];

  systemd.services.rhasspy = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.pamixer pkgs.pulseaudioFull ];
    # rhasspy sets `/dev/stdout` as log file for supervisord
    # supervisord tries to open /dev/stdout and fails with the default systemd device
    # it works for pipes so...
    script = ''
      ${pkgs.nur.repos.mic92.rhasspy}/bin/rhasspy --profile en | ${pkgs.utillinux}/bin/logger
    '';
    serviceConfig = {
      User = "joerg";
      # needed for pulseaudio
      Environment = "XDG_RUNTIME_DIR=/run/user/1000";
    };
  };

  systemd.services.tts = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.tts}/bin/tts-server --model_name tts_models/en/ljspeech/glow-tts --vocoder_name vocoder_models/en/ljspeech/mulitband-melgan
      '';
      User = "joerg";
    };
  };
}
