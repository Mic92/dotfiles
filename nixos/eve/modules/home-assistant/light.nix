{ pkgs, ... }: {
  services.home-assistant.config = {
    shell_command.toggle_light = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@jarvis.r hue-ble-ctl toggle E8:D4:5B:99:49:10
    '';
    intent_script.ToggleLight = {
      speech.text = "Toggled light.";
      action.service = "shell_command.toggle_light";
    };
  };
}
