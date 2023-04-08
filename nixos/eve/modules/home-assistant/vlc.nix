{ pkgs, ... }: {
  services.home-assistant.config = {
    conversation.intents = {
      PlayerStart = [
        "start"
        "play"
      ];
      PlayerPause = [
        "pause"
        "stop"
      ];
      PlayMinimix = [
        "play minimix"
        "play minimix radio"
      ];
      PlayBBC = [
        "play BBC"
        "play BBC radio"
      ];
    };
    sensor = [
      {
        name = "Random minimix";
        platform = "command_line";
        scan_interval = 60 * 60 * 24;
        command = ''${pkgs.curl}/bin/curl -sL https://podcasts.files.bbci.co.uk/p02nrtyg.rss | grep -m1 -oPm1 '(?<=enclosure url=")http://[^"]+.mp3' | shuf -n1 '';
      }
      {
        name = "BBC World News";
        platform = "command_line";
        scan_interval = 60 * 60;
        command = ''${pkgs.curl}/bin/curl -sL https://podcasts.files.bbci.co.uk/p02nq0gn.rss | grep -m1 -oPm1 '(?<=enclosure url=")http://[^"]+.mp3' | head -n1 '';
      }
    ];
    intent_script.PlayerStart = {
      speech.text = "Started";
      action.service = "media_player.media_play";
      action.entity_id = "media_player.vlc_telnet";
    };
    intent_script.PlayerPause = {
      speech.text = "Paused";
      action.service = "media_player.media_pause";
      action.entity_id = "media_player.vlc_telnet";
    };
    intent_script.PlayMinimix = {
      speech.text = "Play minimix";
      action.service = "media_player.play_media";
      action.entity_id = "media_player.vlc_telnet";
      action.data_template.media_content_type = "music";
      action.data_template.media_content_id = "{{ states.sensor.random_minimix.state }}";
    };
    intent_script.PlayBBC = {
      speech.text = "Play B B Ce World News";
      action.service = "media_player.play_media";
      action.entity_id = "media_player.vlc_telnet";
      action.data_template.media_content_type = "music";
      action.data_template.media_content_id = "{{ states.sensor.bbc_world_news.state }}";
    };
    vlc_telnet = { };
  };
}
