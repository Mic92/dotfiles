{ lib, ... }: let
  notifyJoerg = msg: [{
    service = "notify.pushover";
    data_template.message = msg;
  }];
  notifyShannan = msg: [{
    service = "notify.pushover";
    data_template.message = msg;
  }];

  person = entity_id: { to ? null, from ? null }: {
    platform = "state"; inherit entity_id;
  } // lib.optionalAttrs (to != null) { inherit to; }
    // lib.optionalAttrs (from != null) { inherit from; };

  joerg = person "person.jorg_thalheim";
  shannan = person "person.shannan_lekwati";
  maurice = person "person.maurice_baillieu";
  gymTime = {
    condition = "template";
    value_template = ''{{ state_attr("calendar.joerg_shannan_jorg_thalheim", "message") == "Gym" }}'';
  };
in {
   services.home-assistant.config.automation = [{
     alias = "Joerg at Shannan's notification";
     trigger = joerg { to = "Shannan's Home"; };
     action = notifyShannan "Jörg arrived at your place";
   } {
     alias = "Shannan at Joerg's place notification";
     trigger = shannan { to = "home"; };
     action = notifyJoerg "Shannan arrived at your place";
   } {
     alias = "Shannan arrived home notification";
     trigger = shannan { to = "Shannan's Home"; };
     action = notifyJoerg "Shannan is home";
   } {
     alias = "Joerg arrived home notification";
     trigger = joerg { to = "home"; };
     action = notifyShannan "Jörg is home";
   } {
     alias = "Shannan left home notification";
     trigger = shannan { from = "Shannan's Home"; };
     action = notifyJoerg "Shannan left home";
   } {
     alias = "Joerg left home notification";
     trigger = joerg { from = "home"; };
     action = notifyShannan "Jörg left home";
   } {
     alias = "Joerg arrived home notification";
     trigger = joerg { to = "home"; };
     action = notifyShannan "Jörg is home";
   } {
     alias = "Joerg left Uni notification";
     trigger = joerg { from = "University"; };
     action = notifyShannan "Jörg left Uni";
   } {
     alias = "Shannan left work notification";
     trigger = shannan { from = "Work of Shannan"; };
     action = notifyJoerg "Shannan left work";
   } {
     alias = "Joerg arrived at the Gym notification";
     trigger = joerg { to = "Gym"; };
     condition = gymTime;
     action = notifyShannan "Joerg arrived at the Gym";
   } {
     alias = "Shannan arrived at the Gym notification";
     trigger = shannan { to = "Gym"; };
     condition = gymTime;
     action = notifyShannan "Shannan arrived at the Gym";
   } {
     alias = "Maurice left Uni notification";
     trigger = maurice { from = "University"; };
     action = notifyJoerg "Maurice left Uni";
   } {
     alias = "Maurice arrived at Uni notification";
     trigger = maurice { to = "University"; };
     action = notifyJoerg "Maurice arrived at Uni";
   }];
}
