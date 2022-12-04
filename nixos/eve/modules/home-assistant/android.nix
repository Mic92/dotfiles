{
  services.home-assistant.config.intent_script = {
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
