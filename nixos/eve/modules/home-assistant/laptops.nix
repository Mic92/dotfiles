{ config, lib, pkgs, ... }:
{
  services.home-assistant.config = {
    intent_script.SuspendLaptop =  {
      speech.text = "Suspend laptop";
      action.service = "shell_command.suspend_turingmachine";
      action.data_template.host = "turingmachine.r";
    };
    shell_command.suspend_turingmachine =
      ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "sudo /run/current-system/sw/bin/systemctl suspend"'';
  };
}
