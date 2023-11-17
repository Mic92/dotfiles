{ inputs
, pkgs
, ...
}: {
  services.home-assistant.customComponents = [
    (pkgs.stdenv.mkDerivation rec {
      pname = "home-assistant-variables";
      version = "0.15.0";
      src = pkgs.fetchFromGitHub {
        owner = "snarky-snark";
        repo = "home-assistant-variables";
        rev = "v${version}";
        sha256 = "sha256-HKO73B8kARuJxUv8bc0TVvpWQHXeYMe+OncodL8LpP8=";
      };

      installPhase = ''
        cp -r custom_components/var $out
      '';
    })
  ];
  services.home-assistant.customCards = {
    "lovelace-multiline-text-input-card.js" = pkgs.stdenv.mkDerivation {
      name = "lovelace-multiline-text-input-card.js";
      src = pkgs.fetchFromGitHub {
        owner = "faeibson";
        repo = "lovelace-multiline-text-input-card";
        rev = "1.0.4";
        sha256 = "sha256-nuVijc5vuzKlMCjy/DNaZgzNnl11CXlwyLX5H9fNoH4=";
      };

      installPhase = ''
        install -m644 lovelace-multiline-text-input-card.js $out
      '';
    };
  };
  services.home-assistant.config = {
    transmission = [ ];
    var.torrent_url = {
      friendly_name = "Torrent magnet url";
      initial_value = "";
    };

    script.add_torrent.sequence = [
      {
        service = "transmission.add_torrent";
        data_template = {
          name = "Transmission";
          torrent = ''{{ states("var.torrent_url") }}'';
        };
      }
      {
        service = "var.set";
        data.value = "";
        data.entity_id = [ "var.torrent_url" ];
      }
    ];
    automation = [
      {
        alias = "Completed Torrent";
        trigger = {
          platform = "event";
          event_type = "transmission_downloaded_torrent";
        };
        action = [
          {
            service = "notify.irc_flix";
            data_template.message = "torrent completed: {{trigger.event.data.name}}";
          }
          {
            service = "notify.mobile_app_beatrice";
            data_template = {
              title = "Torrent completed!";
              message = ": {{trigger.event.data.name}}";
            };
          }
        ];
      }
    ];
    notify = [
      {
        name = "irc_flix";
        platform = "command_line";
        command = ''${inputs.nur-packages.packages.${pkgs.hostPlatform.system}.ircsink}/bin/ircsink --server=irc.r --nick=transmission --target="#flix"'';
      }
    ];
  };
}
