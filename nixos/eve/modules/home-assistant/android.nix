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
          target = "android";
        };
      };
    };

    FindAndroid = {
      speech.text = "Send notification";
      action = {
        service = "notify.pushover";
        data = {
          message = "Phonefinderalert";
          target = "android";
          data.sound = "echo";
          data.priority = 1;
        };
      };
    };
  };
}
