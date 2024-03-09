let
  notifyDevice = device_name: name: {
    speech.text = "Notified ${name}";
    action = {
      service = "icloud.play_sound";
      data.account = "slekwati@outlook.com";
      data.device_name = device_name;
    };
  };
in
{
  services.home-assistant.config = {
    conversation.intents = {
      FindIphone = [
        "find [(Shannan's|Shannon's)] (phone|IPhone)"
      ];
      FindWatch = [
        "find [(Shannan's|Shannon's)] (watch|Apple Watch)"
      ];
      FindMacbook = [
        "find [(Shannan's|Shannon's)] (laptop|Macbook)"
      ];
    };
    intent_script = {
      FindIphone = notifyDevice "beatrice" "Shannan's phone";
      FindWatch = notifyDevice "shannansapple_watch" "Shannan's watch";
      FindMacbook = notifyDevice "herbert" "Shannan's laptop";
    };
  };
}
