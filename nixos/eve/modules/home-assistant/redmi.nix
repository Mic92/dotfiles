{ config, lib, pkgs, ... }:

{
  services.home-assistant.config.intent_script = {
    PlayPodcast = {
      speech.text = "Start podcast";
      action = {
        service = "notify.pushover";
        data = {
          title = "Tasker";
          message = "Play podcast";
          target = "gt-i9195";
        };
      };
    };

    FindRedmi = {
      speech.text = "Send notification";
      action = {
        service = "notify.pushover";
        data = {
          message = "Phonefinderalert";
          target = "gt-i9195";
          data.sound = "echo";
          data.priority = 1;
        };
      };
    };
  };
}
