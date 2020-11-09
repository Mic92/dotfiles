{ pkgs, ... }: {
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
    12101
  ];

  systemd.services.rhasspy = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
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
    # rhasspy sets `/dev/stdout` as log file for supervisord
    # supervisord tries to open /dev/stdout and fails with the default systemd device
    # it works for pipes so...
    script = ''
      ${pkgs.tts}/bin/tts-server \
        --vocoder_config ./vocoder/config.json \
        --vocoder_checkpoint ./vocoder/checkpoint_1450000.pth.tar \
        --tts_config ./tts/config.json \
        --tts_checkpoint ./tts/checkpoint_130000.pth.tar
    '';
    serviceConfig = {
      User = "joerg";
      # needed for pulseaudio
      Environment = "XDG_RUNTIME_DIR=/run/user/1000";
      WorkingDirectory = "/home/joerg/.config/rhasspy/profiles/en/tts";
    };
  };
}
