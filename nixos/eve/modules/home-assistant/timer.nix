{ config, pkgs, ... }: let
  convertDuration = ''
    {% if unit == "seconds" %}
      {{ duration }}
    {% elif unit == "minutes" %}
      {{ duration * 60 }}
    {% elif unit == "hours" %}
      {{ duration * 60 * 60 }}
    {% endif %}
  '';
in {
  services.home-assistant.config = {
    timer.rhasspy = {};
    timer.pause-rhasspy = {};
    intent = {};
    intent_script = {
      Pause = {
        speech.text = "Suspend Jarvis for {{ duration }} {{ unit }}.";
        action = [{
          service = "timer.start";
          entity_id = "timer.pause-rhasspy";
          data_template.duration = convertDuration;
        } {
          service = "shell_command.ssh_rhasspy_mqtt";
          data_template.state = "Off";
        }];
      };
      GetTime.speech.text = "It is {{ now().hour }}:{{ now().minute }}.";
      GetTimer.speech.text = ''{{ state_attr("timer.rhasspy", "duration") }} is left.'';
      SetTimer = {
        speech.text = "Timer for {{ duration }} {{ unit }} set!";
        action = [{
          service = "timer.start";
          entity_id = "timer.rhasspy";
          data_template.duration = convertDuration;
        }];
      };
      CancelTimer = {
        speech.text = ''Timer for {{ state_attr("timer.rhasspy", "duration") }} canceled.'';
        action = {
          service = "timer.cancel";
          entity_id = "timer.rhasspy";
        };
      };
    };
    rest_command = {
      tts = {
        url = "http://turingmachine.r:12101/api/text-to-speech";
        method = "POST";
        payload = "{{message}}";
      };
    };

    shell_command.ssh_rhasspy_mqtt = let
      cmd = ''mosquitto_pub -L mqtt://localhost:12183/hermes/hotword/toggle{{ state }} -m '{\"siteId\": \"default\", \"reason\": \"\"}'';
    in ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "${cmd}"'';

    automation = [{
      alias = "Timer is up notification";
      trigger = {
        platform = "event";
        event_type = "timer.finished";
        event_data.entity_id = "timer.rhasspy";
      };
      action = [{
        service = "notify.pushover";
        data_template.message = ''timer for {{ state_attr("timer.rhasspy", "duration") }} is up!'';
      } {
        service = "rest_command.tts";
        data_template.message = ''Timer for {{ state_attr("timer.rhasspy", "duration") }} is up!'';
      }];
    } {
      alias = "Timer is up notification";
      trigger = {
        platform = "event";
        event_type = "timer.finished";
        event_data.entity_id = "timer.pause-rhasspy";
      };
      action = [{
        service = "shell_command.ssh_rhasspy_mqtt";
        data_template.state = "On";
      } {
        service = "rest_command.tts";
        data_template.message = ''Re-armed jarvis'';
      }];
    }];
  };
}
