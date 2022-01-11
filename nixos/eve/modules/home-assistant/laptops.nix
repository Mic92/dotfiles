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
      action.data_template.host = "jarvis.r";
    };
    intent_script.UpdateLocation = {
      speech.text = "Updated Shannan's location";
      action.service = "notify.mobile_app_beatrice";
      action.data_template.message = "request_location_update";
    };
    intent_script.PlayMinimix = {
      speech.text = "Play minimix";
      action.service = "shell_command.play_file";
      action.data_template.host = "jarvis.r";
      action.data_template.url = "{{ states.sensor.random_minimix.state }}";
    };
    intent_script.PlayBBC = {
      speech.text = "Play BBC World News";
      action.service = "shell_command.play_file";
      action.data_template.host = "jarvis.r";
      action.data_template.url = "{{ states.sensor.bbc_world_news.state }}";
    };
    shell_command = {
      suspend_laptop =
        ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "sudo /run/current-system/sw/bin/systemctl suspend"'';
      play_file =
        ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} 'sudo -u joerg /etc/profiles/per-user/hass-agent/bin/mpv-play --no-video "{{ url }}"' '';
    };
  };
}
