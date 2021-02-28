{ pkgs, ... }: {
  services.home-assistant.config = {
    shell_command.toggle_light = ''
      ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@turingmachine.r '
        hue-ble-ctl toggle D4:BB:D8:6C:07:86 &
        pid=$!
        hue-ble-ctl toggle E2:71:8C:F7:4F:13 &
        pid2=$!
        # only one light will succeed, kill the other
        wait -n
        kill $pid $pid2'
    '';
    intent_script.ToggleLight = {
      speech.text = "Toggled light.";
      action.service = "shell_command.toggle_light";
    };
  };
}
