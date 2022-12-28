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
          name = "Learn how to code";
          identifier = "learn_to_code";
          actions = [
            {
              identifier = "DONE_PYTHON_ON_MIMO";
              title = "Python on Mimo ";
              activationMode = "background";
            }
            {
              identifier = "LISTENED_TO_JOERG";
              title = "Listened To Jörg";
              activationMode = "background";
            }
            {
              identifier = "READ_SOMETHING_ONLINE";
              title = "Read something about coding online";
              activationMode = "background";
            }
            {
              identifier = "WATCHED_A_YOUTUBE_TUTORIAL";
              title = "Watched a Youtube Tutorial";
              activationMode = "background";
            }
            {
              identifier = "ONLINE_WALKTHROUGH";
              title = "Completed part of an online walkthrough";
              activationMode = "background";
            }
            {
              identifier = "UPLOADED_BLOG";
              title = "Uploaded a blog/done something on github";
              activationMode = "background";
            }
          ];
        }
      ];
      input_number.coding_streak_days = counter "Coding streak";
      input_number.done_python_on_mimo = counter "Days on Mimo streak";
      input_number.listened_to_joerg = counter "Days listened to Jörg streak";
      input_number.read_something_online = counter "Days read something online";
      input_number.watched_a_youtube_tutorial = counter "Days watched something on youtube";
      input_number.online_walkthrough = counter "Days completing online walkthrough";
      input_number.uploaded_blog = counter "Days uploaded a blog or messed around on github";
      input_boolean.learned_coding_today = {
        name = "Did some coding today";
        icon = "mdi:book";
      };
      script =
        let
          doneScript = name: id: {
            alias = name;
            sequence = [
              {
                service = "input_number.increment";
                entity_id = "input_number.coding_streak_days";
              }
              {
                service = "input_number.increment";
                entity_id = "input_number.${id}";
              }
              {
                service = "input_boolean.turn_on";
                entity_id = "input_boolean.learned_coding_today";
              }
              {
                service = "notify.pushover";
                data_template.message = ''
                  Shannan ${name} today. Call her a good girl.
                '';
              }
              {
                service = "notify.mobile_app_beatrice";
                data_template.message = "Great your coding streak is at {{ states.input_number.coding_streak_days.state }} days!";
              }
            ];
          };
        in
        {
          done_python_on_mimo = doneScript "done MIMO" "DONE_PYTHON_ON_MIMO";
          listened_to_joerg = doneScript "Listened to Jörg" "LISTENED_TO_JOERG";
          read_something_online = doneScript "Read something online" "READ_SOMETHING_ONLINE";
          watched_a_youtube_tutorial = doneScript "Watched a youtube tutorial" "WATCHED_A_YOUTUBE_TUTORIAL";
          online_walkthrough = doneScript "Completed an online walkthrough" "ONLINE_WALKTHROUGH";
          uploaded_blog = doneScript "Uploaded a blog/messed around on github" "UPLOADED_BLOG";
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
            alias = "Coding reminder at ${time}";
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
                    Lets get hacky-hacky! (current streak: {{states("input_number.coding_streak_days")}} days)
                  '';
                  data.push.category = "learn_coding";
                };
              }
              {
                service = "notify.pushover";
                data_template.message = ''
                  Remind Shannan to do coding (current streak: {{states("input_number.coding_streak_days")}} days)
                '';
              }
            ];
            condition = [
              {
                condition = "state";
                entity_id = "input_boolean.learned_coding_today";
                state = "off";
              }
              {
                condition = "time";
                weekday = [ "mon" "tue" "wed" "thu" "fri" ];
              }
            ];
          };
        in
        [
          (reminder "12:00:00")
          (doneAutomation "done MIMO" "DONE_PYTHON_ON_MIMO")
          (doneAutomation "Listened to Joerg" "LISTENED_TO_JOERG")
          (doneAutomation "Read something online" "READ_SOMETHING_ONLINE")
          (doneAutomation "Watched a youtube tutorial" "WATCHED_A_YOUTUBE_TUTORIAL")
          (doneAutomation "listened podcast" "DONE_PODCAST")
          (doneAutomation "Uploaded a blog/messed around on github" "UPLOADED_BLOG")
          {
            alias = "Reset learned coding today";
            trigger = {
              platform = "time";
              at = "00:00:01";
            };
            action = [
              {
                service = "input_boolean.turn_off";
                entity_id = "input_boolean.learned_coding_today";
              }
            ];
            condition = [
              {
                condition = "time";
                weekday = [ "sat" "sun" ];
              }
              {
                condition = "state";
                entity_id = "input_boolean.learned_coding_today";
                state = "on";
              }
            ];
          }
          {
            alias = "Break coding learning streak";
            trigger = {
              platform = "time";
              at = "00:00:01";
            };
            action =
              let
                msg = "coding learning streak broke after {{states.input_number.codin_streak_days.state}} days :(";
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
                  entity_id = "input_number.coding_streak_days";
                  data.value = 0;
                }
              ];
            condition = [
              {
                condition = "state";
                entity_id = "input_boolean.learned_coding_today";
                state = "off";
              }
              {
                condition = "time";
                weekday = [ "sat" "sun" ];
              }
            ];
          }
        ];
    };
}
