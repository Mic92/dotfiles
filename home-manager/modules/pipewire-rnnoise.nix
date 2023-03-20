{ pkgs, ... }:
{
  # USAGE:
  # use qpwgraph to connect the input of the noise canceling source to the output of the microphone
  # Than configure the rrnoise source as the default input device in your application.
  #
  # EXAMPLE: See also https://github.com/Mic92/dotfiles/releases/download/assets/20230320-213902.png
  home.file.".config/pipewire/pipewire.conf.d/99-input-denoising.conf" = {
    source = (pkgs.formats.json { }).generate "99-input-denoising.conf" {
      "context.modules" = [
        {
          "name" = "libpipewire-module-filter-chain";
          "args" = {
            "node.description" = "Noise Canceling source";
            "media.name" = "Noise Canceling source";
            "filter.graph" = {
              "nodes" = [
                {
                  "type" = "ladspa";
                  "name" = "rnnoise";
                  "plugin" = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                  "label" = "noise_suppressor_stereo";
                  "control" = {
                    "VAD Threshold (%)" = 50.0;
                  };
                }
              ];
            };
            "audio.position" = [ "FL" "FR" ];
            "capture.props" = {
              "node.name" = "effect_input.rnnoise";
              "node.passive" = true;
            };
            "playback.props" = {
              "node.name" = "effect_output.rnnoise";
              "media.class" = "Audio/Source";
            };
          };
        }
      ];
    };
  };
}
