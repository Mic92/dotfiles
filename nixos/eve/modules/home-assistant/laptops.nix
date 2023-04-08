{ config
, pkgs
, ...
}: {
  services.home-assistant.config = {
    conversation.intents = {
      SuspendLaptop = [
        "Suspend [my] laptop"
      ];
      SuspendBernie = [
        "Suspend Bernie"
      ];
      UpdateShannansLocation = [
        "Update Shannan's location"
      ];
      UpdateJoergsLocation = [
        "Update York's location"
      ];
    };
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
    intent_script.UpdateShannansLocation = {
      speech.text = "Updated Shannan's location";
      action.service = "icloud.update";
    };
    intent_script.UpdateJoergsLocation = {
      speech.text = "Updated Yorks's location";
      action.service = "notify.mobile_app_android";
      action.data.message = "request_location_update";
    };
    shell_command = {
      suspend_laptop = ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "sudo /run/current-system/sw/bin/systemctl suspend"'';
    };
  };
}
