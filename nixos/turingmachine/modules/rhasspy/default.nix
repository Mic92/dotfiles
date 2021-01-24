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

  systemd.services.tts = let
    server = pkgs.stdenv.mkDerivation {
      name = "tts-server";
      dontUnpack = true;
      nativeBuildInputs = [
        pkgs.python3.pkgs.wrapPython
      ];
      propagatedBuildInputs = [ pkgs.tts pkgs.python3.pkgs.flask ];

      installPhase = ''
        install -D -m755 ${./server.py} $out/bin/tts-server
      '';
      postFixup = "wrapPythonPrograms";
    };
  in {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${server}/bin/tts-server \
          --tts-config ./config.json \
          --tts-checkpoint ./tts_model.pth.tar \
          --vocoder-config ./config_vocoder.json \
          --vocoder-checkpoint ./vocoder_model.pth.tar
      '';
      User = "joerg";
      WorkingDirectory = "/home/joerg/.config/rhasspy/profiles/en/tts";
    };
  };
}
