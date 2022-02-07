{ pkgs, ... }: let
  rhasspy-play = pkgs.writeScriptBin "rhasspy-play" ''
    #!${pkgs.runtimeShell}
    set -eux -o pipefail
    export PATH=${pkgs.pipewire}/bin:${pkgs.pamixer}/bin
    sink=alsa_output.usb-SEEED_ReSpeaker_4_Mic_Array__UAC1.0_-00.analog-stereo

    pamixer --set-volume 34 --sink "$sink"
    if pamixer --get-mute --sink="$sink"; then
        pamixer --sink=$sink --unmute
        pw-play /dev/stdin
        pamixer --sink=$sink --mute
    else
        pw-play /dev/stdin
    fi
  '';
in {
  imports = [
    ../../../modules/pipewire.nix
  ];
  #hardware.pulseaudio.enable = true;
  #systemd.user.sockets.pipewire-pulse = {
  #  socketConfig.ListenStream = "4713";
  #};

  #services.pipewire.config.pipewire-pulse = {
  #  "context.modules" = [{
  #    args = {};
  #    flags = [ "ifexists" "nofail" ];
  #    name = "libpipewire-module-rtkit";
  #  }
  #  {
  #    name = "libpipewire-module-protocol-native";
  #  }
  #  {
  #    name = "libpipewire-module-client-node";
  #  } {
  #    name = "libpipewire-module-adapter";
  #  } {
  #    name = "libpipewire-module-metadata";
  #  }
  #  {
  #    args = {
  #      # the addresses this server listens on
  #      "server.address" = [
  #        "unix:native"
  #        #"tcp:4713"
  #      ];
  #      "vm.overrides" = { "pulse.min.quantum" = "1024/48000"; };
  #    };
  #    name = "libpipewire-module-protocol-pulse";
  #  }
  #  ];
  #};
  # allow eve/turingmachine
  networking.firewall.extraCommands = ''
    ip6tables -I nixos-fw -p tcp -s 42:0:3c46:70c7:8526:2adf:7451:8bbb --dport 12101 -j nixos-fw-accept
    ip6tables -I nixos-fw -p tcp -s 42:0:3c46:47e8:f610:15d1:27a3:674b --dport 12101 -j nixos-fw-accept
  '';

  environment.systemPackages = [
    rhasspy-play
    pkgs.pavucontrol
  ];

  services.getty.autologinUser = "joerg";

  systemd.user.services.rhasspy = {
    wantedBy = [ "default.target" ];
    path = [ pkgs.pamixer pkgs.pulseaudioFull rhasspy-play ];
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
}
