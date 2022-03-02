{
  services.home-assistant.config.intent_script = {
    Noop.speech.text = "";
    Psst.speech.text = "Wot.";
    ThankYou.speech.text = "You are welcome.";
    FuckOff.speech.text = "Fuck yourself.";
    NotAGirl.speech.text = "Not a girl!";
    Drunk.speech.text = "tv";
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
}
