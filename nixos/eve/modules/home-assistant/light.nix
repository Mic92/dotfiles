{ pkgs, ... }: {
  services.home-assistant.config = {
    conversation.intents = {
      ToggleLight = [
        "Toggle light"
        "Turn light on"
        "Turn light off"
      ];
    };
    light = {
      platform = "template";
      lights.bedroom = {
        friendly_name = "Sofa light";
        turn_on.service = "shell_command.turn_light_on";
        turn_off.service = "shell_command.turn_light_off";
      };
    };
    shell_command.toggle_light = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@jarvis.r hue-ble-ctl toggle E8:D4:5B:99:49:10
    '';
    shell_command.turn_light_on = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@jarvis.r hue-ble-ctl switch_on E8:D4:5B:99:49:10
    '';
    shell_command.turn_light_off = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@jarvis.r hue-ble-ctl switch_off E8:D4:5B:99:49:10
    '';
    intent_script.ToggleLight = {
      speech.text = "Toggled light.";
      action.service = "shell_command.toggle_light";
    };
  };
}
