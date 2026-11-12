;;; Routes and places from gmaps-cli.

(in-package #:va-tests)

(define-test travel-phrasings
  :parent va-tests
  (parses-as "Travel(Route { origin: ~a, dest: ~s, mode: ~s })"
             '(("When is the next train to Laim?" "None" "laim" "transit")
               ("What's the next connection to the airport?" "None" "the airport" "transit")
               ("Next bus to Pasing" "None" "pasing" "transit")
               ("When does the next train to Berlin leave?" "None" "berlin" "transit")
               ("When is the next train from Laim to the airport?" "Some(\"laim\")" "the airport"
                "transit")
               ("How do I get to the main station?" "None" "the main station" "transit")
               ("How do I get to Berlin by car?" "None" "berlin" "driving")
               ("Directions to Marienplatz" "None" "marienplatz" "transit")
               ("How long does it take to drive to Berlin?" "None" "berlin" "driving")
               ("How long does it take to walk to the main station?" "None" "the main station"
                "walking")
               ("How long would it take me to cycle to Pasing?" "None" "pasing" "bicycling")
               ("How long does it take to get to Berlin?" "None" "berlin" "transit")
               ("How long to the airport by train?" "None" "the airport" "transit")
               ("How long is it to Garching on foot?" "None" "garching" "walking")
               ("How long from Laim to the airport by car?" "Some(\"laim\")" "the airport"
                "driving")
               ("How far is it to Berlin?" "None" "berlin" "driving")
               ("How far is it from Munich to Hamburg?" "Some(\"munich\")" "hamburg" "driving")
               ("How far is the airport from here?" "None" "the airport" "driving")))
  (parses-as "Travel(Nearby { what: ~s, near: ~a })"
             '(("Find a pharmacy nearby" "pharmacy" "None")
               ("Where is the nearest supermarket?" "supermarket" "None")
               ("Is there a bakery near me?" "bakery" "None")
               ("Closest ATM" "atm" "None")
               ("Find me some coffee shops around here" "coffee shops" "None")
               ("Where is the closest gas station to Glendale?" "gas station" "Some(\"glendale\")")
               ("Find the nearest pharmacy in Pasing" "pharmacy" "Some(\"pasing\")")
               ("Where is the nearest place to eat?" "place to eat" "None")))
  (parses-not "Travel(" '("How long to boil an egg?"
                          "How long until my next meeting?"
                          "How long is a marathon?"
                          "How far is the moon?"
                          "What's the next holiday?"
                          "How do I get to sleep?")))

(defun walk (minutes)
  (va::obj "instruction" (format nil "Head east~%Destination will be on the left")
           "distance" "0.2 km" "duration" (format nil "~d mins" minutes)))

(defun ride (vehicle line from departs to arrives)
  (va::obj "instruction" "Towards somewhere" "distance" "4.6 km" "duration" "8 mins"
           "transit" (va::obj "line" line "vehicle" vehicle "departure_stop" from
                              "departure_time" departs "arrival_stop" to "arrival_time" arrives)))

(defun route-output (metres seconds distance duration &rest steps)
  "What `gmaps-cli route --json` prints."
  (com.inuoe.jzon:stringify (va::obj "distance_meters" metres "duration_seconds" seconds
                                     "distance" distance "duration" duration
                                     "url" "https://www.google.com/maps/dir/?api=1"
                                     "steps" (coerce steps 'vector))))

(defun route-json (&rest args) (va::parse-route (apply #'route-output args)))

(defun transit ()
  "One change, a walk at either end."
  (route-json 5121 1050 "5.1 km" "17 mins"
              (walk 1) (walk 2)
              (ride "SUBWAY" "U5" "Munich Central Station" "2026-11-12T22:46:31Z" "Laimer Platz"
                    "2026-11-12T22:54:41Z")
              (ride "BUS" "57" "Laimer Platz" "2026-11-12T22:58:00Z" "Willibaldplatz"
                    "2026-11-12T23:01:00Z")
              (walk 2)))

(defun driving () (route-json 590213 23100 "590.2 km" "6 hours 25 mins" (walk 1) (walk 3)))

(defun clock (iso) (subseq iso 11 16))

(define-test travel-routes-are-read
  :parent va-tests
  (is = 2 (length (va::rides (transit))))
  (is = 5 (length (va::steps (transit))))
  (false (va::parse-route "{\"error\": \"No route found from 'a' to 'b'\"}"))
  (false (va::parse-route "No route found from 'a' to 'b'"))
  (false (va::parse-route "")))

(define-test travel-speech
  :parent va-tests
  (is equal (format nil "Take the U5 from Munich Central Station at 22:46, then the 57. ~
One change, 17 minutes door to door.")
      (va::trip-speech (transit) "laim" "transit" #'clock))
  (is equal "Berlin is 590 kilometres away, about 6 hours and 25 minutes by car."
      (va::trip-speech (driving) "berlin" "driving" #'clock))
  (is equal "The airport is 590 kilometres away, about 6 hours and 25 minutes by bike."
      (va::trip-speech (driving) "the airport" "bicycling" #'clock))
  ;; No ride in it: public transport was asked for, but it is a walk.
  (is equal "Laim is 590 kilometres away, about 6 hours and 25 minutes by public transport."
      (va::trip-speech (driving) "laim" "transit" #'clock))
  (is equal "To the airport" (va::trip-title "the airport"))
  (is equal "To Munich International Airport" (va::trip-title "Munich International Airport"))
  (is equal "850 metres" (va::spoken-distance 852))
  (is equal "5.1 kilometres" (va::spoken-distance 5121)))

(define-test travel-view
  :parent va-tests
  (let* ((view (va::trip-view (transit) "laim" "transit" #'clock))
         (items (gethash "items" view)))
    (is equal "timeline" (gethash "kind" view))
    (is equal "To Laim" (gethash "title" view))
    (is equal "17 mins · 5.1 km · by public transport" (gethash "subtitle" view))
    ;; Walking legs fold into one item each.
    (is equal '("Walk" "U5" "57" "Walk") (map 'list (lambda (i) (gethash "title" i)) items))
    (is equal '("walk" "train" "bus" "walk") (map 'list (lambda (i) (gethash "glyph" i)) items))
    (is equal '("3 min" "22:46 – 22:54" "22:58 – 23:01" "2 min")
        (map 'list (lambda (i) (gethash "meta" i)) items))
    (is equal "Munich Central Station → Laimer Platz" (gethash "detail" (elt items 1))))
  (let ((items (gethash "items" (va::trip-view (driving) "berlin" "driving" #'clock))))
    (is equal "Head east" (gethash "title" (elt items 0)))
    (is equal "0.2 km · 1 mins" (gethash "meta" (elt items 0)))))

(define-test travel-needs-a-home
  :parent va-tests
  (let ((va::*home-file* #p"/nonexistent/home"))
    (false (va::fake-effects
            (true (search "/nonexistent/home"
                          (va::reply-body (va::route nil "berlin" "driving")))))))
  (uiop:with-temporary-file (:pathname home :stream out)
    (write-line "Somestreet 1, Munich" out)
    (finish-output out)
    (let ((va::*home-file* home))
      (is equal '(("gmaps-cli" "route" "Somestreet 1, Munich" "berlin" "--mode" "driving"
                   "--json"))
          (remove "date" (va::fake-effects (va::route nil "berlin" "driving"))
                  :key #'va::car-safe :test #'equal))
      (is equal '(("gmaps-cli" "route" "laim" "berlin" "--mode" "transit" "--json"))
          (remove "date" (va::fake-effects (va::route "laim" "berlin" "transit"))
                  :key #'va::car-safe :test #'equal))
      ;; A landmark is looked up near the start first.
      (is equal '(("gmaps-cli" "nearby" "airport" "--location" "Somestreet 1, Munich" "--limit"
                   "1" "--json")
                  ("gmaps-cli" "route" "Somestreet 1, Munich" "the airport" "--mode" "transit"
                   "--json"))
          (va::fake-effects (va::route nil "the airport" "transit")))
      (is equal '(("gmaps-cli" "nearby" "pharmacy" "--location" "Somestreet 1, Munich"
                   "--limit" "5" "--json"))
          (va::fake-effects (va::nearby "pharmacy" nil)))
      (is equal '(("gmaps-cli" "nearby" "pharmacy" "--location" "pasing" "--limit" "5" "--json"))
          (va::fake-effects (va::nearby "pharmacy" "pasing"))))))

(defun places ()
  (va::parse-places
   (com.inuoe.jzon:stringify
    (va::obj "places"
             (vector (va::obj "name" "Wittelsbacher-Apotheke" "rating" 4.6d0
                              "address" "Lindwurmstraße 97, 80337 München, Germany"
                              "url" "https://www.google.com/maps/search/?api=1&query=W")
                     (va::obj "name" "Winthir pharmacy"
                              "address" "Nymphenburger Str. 160, 80634 München, Germany"
                              "url" "https://www.google.com/maps/search/?api=1&query=X"))))))

(define-test travel-places
  :parent va-tests
  (let ((places (places)))
    (is = 2 (length places))
    (is equal "Wittelsbacher-Apotheke, Lindwurmstraße 97. Also Winthir pharmacy."
        (va::places-speech places))
    (is equal "Wittelsbacher-Apotheke, Lindwurmstraße 97."
        (va::places-speech (list (first places))))
    (let* ((view (va::places-view places "pharmacy" nil))
           (items (gethash "items" view)))
      (is equal "list" (gethash "kind" view))
      (is equal "Pharmacy nearby" (gethash "title" view))
      (is equal "Pharmacy near Pasing" (va::places-title "pharmacy" "pasing"))
      (is equal "★ 4.6" (gethash "meta" (elt items 0)))
      (false (gethash "meta" (elt items 1)))
      (is equal "https://www.google.com/maps/search/?api=1&query=X" (gethash "url" (elt items 1)))))
  (false (va::parse-places "{\"error\": \"Could not find location: x\"}"))
  (false (va::parse-places "")))

(define-test travel-shows-instead-of-notifying
  :parent va-tests
  (uiop:with-temporary-file (:pathname home :stream out)
    (write-line "Somestreet 1, Munich" out)
    (finish-output out)
    (let ((va::*home-file* home)
          (va::*fake-output*
           (list (cons "gmaps-cli" (route-output 590213 23100 "590.2 km" "6 hours 25 mins"
                                                 (walk 1))))))
      (conversation
       (let ((r (hear "how long does it take to drive to berlin")))
         (is equal "Berlin is 590 kilometres away, about 6 hours and 25 minutes by car."
             (got r "speech"))
         (false (got r "notify"))
         (false (got r "cache"))))
      (is equal "To Berlin"
          (gethash "title" (second (find :view (va::fake-effects (va::route nil "berlin" "driving"))
                                         :key #'va::car-safe)))))
    ;; Without a route there is nothing on screen, so the notification stays.
    (let ((va::*home-file* home))
      (conversation (true (got (hear "how long does it take to drive to berlin") "notify"))))))
