{ config, lib, pkgs, ... }:
{
  services.home-assistant.config = {
    intent_script.SuspendLaptop = {
      speech.text = "Suspend laptop";
      action.service = "shell_command.suspend_laptop";
      action.data_template.host = "turingmachine.r";
    };
    intent_script.SuspendBernie = {
      speech.text = "Suspend laptop";
      action.service = "shell_command.suspend_laptop";
      action.data_template.host = "bernie.r";
    };
    intent_script.UpdateLocation = {
      speech.text = "Updated Shannan's location";
      action.service = "icloud.update";
    };
    shell_command = {
      suspend_laptop =
        ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "sudo /run/current-system/sw/bin/systemctl suspend"'';
    };
  };
}
