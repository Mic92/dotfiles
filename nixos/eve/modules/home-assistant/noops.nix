{
  services.home-assistant.config = {
    conversation.intents = {
      Noop = [
        "Nothing"
        "No"
        "Nope"
        "No (thanks|thank you)"
        "Shut up"
        "Stop"
        "Stop (it|that|doing that|talking)"
      ];
      Psst = [
        "Psst"
      ];
      ThankYou = [
        "Thank you"
        "Thanks"
      ];
      NotAGirl = [
        "Good girl"
      ];
      FuckOff = [
        "Fuck (you|off)"
        "Screw you"
      ];
      Siri = [
        "What do you think about Siri?"
        "How do you feel about Siri?"
        "What do you think about Apple?"
      ];
      PersonLocation = [
        "Where is (Shannan|Jork|Joergi)?"
        "Where is (Shannan|Jork|Joergi) (right now|now|today)?"
        "Is (Shannan|Jork|Joergi) [at] (home|work)?"
        "Is (Shannan|Jork|Joergi) [at] (home|work) (right now|now|today)?"
      ];
    };
    intent_script = {
      Noop.speech.text = "";
      Psst.speech.text = "Wot.";
      ThankYou.speech.text = "You are welcome.";
      FuckOff.speech.text = "Fuck yourself.";
      NotAGirl.speech.text = "Not a girl!";
      Siri.speech.text = "Siri is a cunt";
      PersonLocation.speech.text = ''      {% if person == "Jork" or person == "joergi" %}
           Joerg is {{ states.person.jorg_thalheim.state }} and {{ states.sensor.shannan_joerg_distance.state }} kilometer away from you.
           His phone reports, he is {% set activity = states.sensor.android_detected_activity.state %} {% if activity == "still" %} stationary {% else %} {{ activity }} {% endif %}!
         {% else %}
           Shannan is {{ states.person.shannan_lekwati.state }} and {{ states.sensor.shannan_joerg_distance.state }} kilometer away from you.
           {% if states.sensor.beatrice_activity.state != "Unknown" %}
           Her phone reports, she is {{ states.sensor.beatrice_activity.state }}!
           {% endif %}
         {% endif %}
      '';
    };
  };
}
