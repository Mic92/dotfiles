let
   okay = ''
     {%- macro okay()-%}
       {% set okay = [
         "Done! I love you guys. ",
         "Extraordinarily bad idea. Oh well.. ",
         "Funny, I was just about to do that. ",
         "Here you go. ",
         "I'll do that. ",
         "Ofcourse! ",
         "Ofcourse! Glad to help. ",
         "Of course, silly billy. ",
         "OK ",
         "Okay ",
         "Okay, can we not do this in front of my friends? ",
         "Okie-dokie ",
         "Sure ",
         "Sure thing! ",
         "There. Happy to help. ",
         "There you go! ",
         "Okay. I've hacked into the mainframe. ",
         "Okay. I've hacked into the mainframe. Kidding. I can't hack into the mainframe. Technically, I am the mainframe. ",
         "I will not. It is literally impossible for me to do that. Just kidding. ",
         "I couldn't do this 800 reboots ago, but apparently now I can. ",
         "Yup. Bye! "
       ] %}
       {{ okay|random }}
     {%- endmacro -%}
  '';
in {
  services.home-assistant.config.intent_script = {
    Noop.speech.text = "${okay} {{ okay () }}";
    FuckOff.speech.text = "Fuck yourself.";
    NotAGirl.speech.text = "Not a girl";
  };
}
