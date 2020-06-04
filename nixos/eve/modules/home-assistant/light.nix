{ pkgs, ...}: {
  services.home-assistant.config = let
    lightCmd = address: ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@turingmachine.r "hue-ble-ctl toggle ${address}"
    '';
  in {
    shell_command.toggle_light_joerg = lightCmd "D4:BB:D8:6C:07:86";
    intent_script = {
      ToggleJoergsLight = {
        speech.text = "Toggled Yörgs light.";
        action.service = "shell_command.toggle_light_joerg";
      };
    };

    shell_command.toggle_light_shannan = lightCmd "E2:71:8C:F7:4F:13";
    intent_script = {
      ToggleShannansLight = {
        speech.text = "Toggled Shannans light.";
        action.service = "shell_command.toggle_light_shannan";
      };
    };
  };
}
