{
  services.home-assistant.config = {
    timer.rhasspy = {};
    intent = {};
    intent_script = {
      GetTime.speech.text = "It is {{ now().hour }}:{{ now().minute }}.";
      GetTimer.speech.text = ''{{ state_attr("timer.rhasspy", "duration") }} is left.'';
      SetTimer = {
        speech.text = "Set a timer for {{ duration }} {{ unit }}.";
        action = [{
          service = "timer.start";
          entity_id = "timer.rhasspy";
          data_template.duration = ''{% if unit == "seconds" %}
              {{ duration }}
            {% elif unit == "minutes" %}
              {{ duration * 60 }}
            {% elif unit == "hours" %}
              {{ duration * 60 * 60 }}
            {% endif %}
          '';
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
    }];
  };
}
