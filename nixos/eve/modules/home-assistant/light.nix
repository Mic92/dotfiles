{ pkgs, ...}: {
  services.home-assistant.config = {
    shell_command.toggle_light = ''${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@turingmachine.r "hue-ble-ctl toggle D4:BB:D8:6C:07:86"'';
    intent_script = {
      ToggleLight = {
        speech.text = "Toggled the light switch.";
        action.service = "shell_command.toggle_light";
      };
    };
  };
}
