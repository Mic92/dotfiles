{ pkgs, ... }: {
  services.home-assistant.config = {
    shell_command.toggle_light = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@turingmachine.r hue-ble-ctl toggle CC:66:33:AD:53:1D
    '';
    intent_script.ToggleLight = {
      speech.text = "Toggled light.";
      action.service = "shell_command.toggle_light";
    };
  };
}
