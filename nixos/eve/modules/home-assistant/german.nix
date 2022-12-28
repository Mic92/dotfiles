{ lib, ... }: {
  services.home-assistant.config =
    let
      counter = name: {
        inherit name;
        min = 0;
        max = 999;
        step = 1;
        icon = "mdi:book";
      };
    in
    {
      ios.push.categories = [
        {
          name = "Learn German";
          identifier = "learn_german";
          actions = [
            {
              identifier = "DONE_DUOLINGO";
              title = "Duolingo :)";
              activationMode = "background";
            }
            {
              identifier = "DONE_GRAMMAR";
              title = "Grammar :)";
              activationMode = "background";
            }
            {
              identifier = "DONE_BOOK";
              title = "Book :)";
              activationMode = "background";
            }
            {
              identifier = "DONE_PODCAST";
              title = "Podcast :)";
              activationMode = "background";
            }
            {
              identifier = "DONE_MEDIA";
              title = "Media :)";
              activationMode = "background";
            }
            {
              identifier = "DONE_SPEAKING";
              title = "Speaking :)";
              activationMode = "background";
            }
          ];
        }
      ];
      input_number.german_streak_days = counter "German learning streak";
      input_number.done_duolingo = counter "Days Duolingo";
      input_number.done_grammar = counter "Days grammar";
      input_number.done_book = counter "Days book";
      input_number.done_podcast = counter "Days podcast";
      input_number.done_media = counter "Days media";
      input_number.done_speaking = counter "Days speaking";
      input_boolean.learned_german_today = {
        name = "Learned German today";
        icon = "mdi:book";
      };
      script =
        let
          doneScript = name: id: {
            alias = name;
            sequence = [
              {
                service = "input_number.increment";
                entity_id = "input_number.german_streak_days";
              }
              {
                service = "input_number.increment";
                entity_id = "input_number.${id}";
              }
              {
                service = "input_boolean.turn_on";
                entity_id = "input_boolean.learned_german_today";
              }
              {
                service = "notify.pushover";
                data_template.message = ''
                  Shannan ${name} today. Call her a good girl.
                '';
              }
              {
                service = "notify.mobile_app_beatrice";
                data_template.message = "Great your German streak is at {{ states.input_number.german_streak_days.state }} days!";
              }
            ];
          };
        in
        {
          done_duolingo = doneScript "done Duolingo" "DONE_DUOLINGO";
          done_grammar = doneScript "done grammar" "DONE_GRAMMAR";
          done_speaking = doneScript "done speaking" "DONE_SPEAKING";
          done_book = doneScript "read a book" "DONE_BOOK";
          done_podcast = doneScript "listened podcast" "DONE_PODCAST";
          done_media = doneScript "listened/watched media" "DONE_MEDIA";
        };
      automation =
        let
          doneAutomation = name: id: {
            alias = name;
            trigger = {
              platform = "event";
              event_type = "ios.notification_action_fired";
              event_data.actionName = id;
            };
            action.service = "script.${lib.toLower id}";
          };
          reminder = time: {
            alias = "German reminder at ${time}";
            trigger = {
              platform = "time";
              at = time;
            };
            action = [
              {
                service = "notify.mobile_app_beatrice";
                data_template = {
                  title = "Hey Shannan!";
                  message = ''
                    How about some German today? (current streak: {{states("input_number.german_streak_days")}} days)
                  '';
                  data.push.category = "learn_german";
                };
              }
              {
                service = "notify.pushover";
                data_template.message = ''
                  Remind Shannan to do German (current streak: {{states("input_number.german_streak_days")}} days)
                '';
              }
            ];
            condition = [
              {
                condition = "state";
                entity_id = "input_boolean.learned_german_today";
                state = "off";
              }
              {
                condition = "time";
                weekday = [ "mon" "wed" "fri" ];
              }
            ];
          };
        in
        [
          (reminder "19:05:00")
          (doneAutomation "done Duolingo" "DONE_DUOLINGO")
          (doneAutomation "done grammar" "DONE_GRAMMAR")
          (doneAutomation "done speaking" "DONE_SPEAKING")
          (doneAutomation "read a book" "DONE_BOOK")
          (doneAutomation "listened podcast" "DONE_PODCAST")
          (doneAutomation "listened/watched media" "DONE_MEDIA")
          {
            alias = "Reset learned German today";
            trigger = {
              platform = "time";
              at = "00:00:01";
            };
            action = [
              {
                service = "input_boolean.turn_off";
                entity_id = "input_boolean.learned_german_today";
              }
            ];
            condition = [
              {
                condition = "time";
                weekday = [ "tue" "thu" "sat" ];
              }
              {
                condition = "state";
                entity_id = "input_boolean.learned_german_today";
                state = "on";
              }
            ];
          }
          {
            alias = "Break German learning streak";
            trigger = {
              platform = "time";
              at = "00:00:01";
            };
            action =
              let
                msg = "German learning streak broke after {{states.input_number.german_streak_days.state}} days :(";
              in
              [
                {
                  service = "notify.mobile_app_beatrice";
                  data_template.message = msg;
                }
                {
                  service = "notify.pushover";
                  data_template.message = msg;
                }
                {
                  service = "input_number.set_value";
                  entity_id = "input_number.german_streak_days";
                  data.value = 0;
                }
              ];
            condition = [
              {
                condition = "state";
                entity_id = "input_boolean.learned_german_today";
                state = "off";
              }
              {
                condition = "time";
                weekday = [ "tue" "thu" "sat" ];
              }
            ];
          }
        ];
    };
}
