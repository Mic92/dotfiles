{ ... }: {
   services.home-assistant.config.automation = [{
     alias = "Joerg at Shannan's notification";
     trigger = {
       platform = "state";
       entity_id  = "person.jorg_thalheim";
       to = "Shannan's Home";
     };
     action = [{
       service = "notify.mobile_app_beatrice";
       data_template.message = "Jörg arrived at your place";
     }];
   } {
     alias = "Shannan at Joerg's place notification";
     trigger = {
       platform = "state";
       entity_id  = "person.shannan_lekwati";
       to = "home";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Shannan arrived at your place";
     }];
   } {
     alias = "Shannan arrived home notification";
     trigger = {
       platform = "state";
       entity_id  = "person.shannan_lekwati";
       to = "Shannan's Home";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Shannan is home";
     }];
   } {
     alias = "Joerg arrived home notification";
     trigger = {
       platform = "state";
       entity_id  = "person.jorg_thalheim";
       to = "home";
     };
     action = [{
       service = "notify.mobile_app_beatrice";
       data_template.message = "Jörg is home";
     }];
   } {
     alias = "Shannan left home notification";
     trigger = {
       platform = "state";
       entity_id  = "person.shannan_lekwati";
       from = "home";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Shannan left home";
     }];
   } {
     alias = "Joerg left home notification";
     trigger = {
       platform = "state";
       entity_id  = "person.jorg_thalheim";
       from = "home";
     };
     action = [{
       service = "notify.mobile_app_beatrice";
       data_template.message = "Jörg left home";
     }];
   } {
     alias = "Joerg arrived home notification";
     trigger = {
       platform = "state";
       entity_id  = "person.jorg_thalheim";
       to = "home";
     };
     action = [{
       service = "notify.mobile_app_beatrice";
       data_template.message = "Jörg is home";
     }];
   } {
     alias = "Joerg left Uni notification";
     trigger = {
       platform = "state";
       entity_id  = "person.jorg_thalheim";
       from = "University";
     };
     action = [{
       service = "notify.mobile_app_beatrice";
       data_template.message = "Jörg left Uni";
     }];
   } {
     alias = "Shannan left work notification";
     trigger = {
       platform = "state";
       entity_id  = "person.shannan_lekwati";
       from = "Work of Shannan";
       to = "not_home";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Shannan left work";
     }];
   } {
     alias = "Maurice left Uni notification";
     trigger = {
       platform = "state";
       entity_id  = "person.maurice_baillieu";
       from = "University";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Maurice left Uni";
     }];
   } {
     alias = "Maurice arrived at Uni notification";
     trigger = {
       platform = "state";
       entity_id  = "person.maurice_baillieu";
       to = "University";
     };
     action = [{
       service = "notify.pushover";
       data_template.message = "Maurice arrived at Uni";
     }];
   }];
}
