{ pkgs, ... }: {
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
    12101
  ];

  systemd.user.services.rhasspy = {
    wantedBy = [ "default.target" ];
    path = [ pkgs.pamixer pkgs.pulseaudioFull ];
    # rhasspy sets `/dev/stdout` as log file for supervisord
    # supervisord tries to open /dev/stdout and fails with the default systemd device
    # it works for pipes so...
    script = ''
      ${pkgs.nur.repos.mic92.rhasspy}/bin/rhasspy --http-host '[::]' --profile en | ${pkgs.utillinux}/bin/logger
    '';
    serviceConfig.Environment = [
      "ALSA_PLUGIN_DIRS=${pkgs.alsaPlugins}/lib/alsa-lib"
      "XDG_RUNTIME_DIR=/run/user/1000"
    ];
  };

  systemd.services.tts = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.tts}/bin/tts-server --model_name tts_models/en/ljspeech/tacotron2-DDC --vocoder_name vocoder_models/en/ljspeech/hifigan_v2
      '';
      User = "joerg";
    };
  };
}
