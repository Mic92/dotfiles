;;; Routes, connections and nearby places from `gmaps-cli --json`.

(in-package #:va)

(in-skill travel)

(defvar *home-file* nil
  "The file with the home address on its first line, or nil for
voice-assistant/home in the config directory. Routes start there.")

(defn home-path () pathname
  (or *home-file*
      (merge-pathnames "voice-assistant/home"
                       (uiop:ensure-directory-pathname
                        (or (uiop:getenv-absolute-directory "XDG_CONFIG_HOME")
                            (merge-pathnames ".config/" (user-homedir-pathname)))))))

(defn home () (or null string)
  (handler-case
      (with-open-file (in (home-path) :if-does-not-exist nil)
        (let ((line (and in (read-line in nil))))
          (and line (plusp (length (trim line))) (trim line))))
    (file-error () nil)))

(defparameter *modes*
  '((("by" "car") . "driving") (("driving") . "driving") (("by" "taxi") . "driving")
    (("by" "bike") . "bicycling") (("by" "bicycle") . "bicycling") (("cycling") . "bicycling")
    (("on" "foot") . "walking") (("by" "foot") . "walking") (("walking") . "walking")
    (("by" "train") . "transit") (("by" "bus") . "transit") (("by" "tram") . "transit")
    (("by" "subway") . "transit") (("by" "transit") . "transit")
    (("by" "public" "transport") . "transit") (("with" "public" "transport") . "transit")
    (("on" "a" "bus") . "transit") (("on" "the" "bus") . "transit")
    (("on" "a" "train") . "transit") (("on" "the" "train") . "transit"))
  "How a sentence ends when it names the way to travel, and gmaps-cli's mode.")

(defparameter *travel-verbs*
  '(("drive" . "driving") ("walk" . "walking") ("cycle" . "bicycling") ("bike" . "bicycling")
    ("ride" . "bicycling") ("get" . nil) ("go" . nil) ("travel" . nil) ("commute" . nil))
  "Verbs before \"to\" or \"from\", and the mode they imply.")

(defparameter *not-destinations*
  '("sleep" "bed" "work" "know" "you" "it" "there" "here" "me")
  "\"How do I get to sleep\" is no trip.")

(defn ends-with ((ws words) (tail list)) boolean
  (let ((n (- (length ws) (length tail))))
    (and (>= n 0) (equal (nthcdr n ws) tail))))

(defn parse-trip ((text string) (default string) &key (sure boolean nil)) list
  "\"to drive to berlin\", \"from laim to the airport by train\": the origin
or nil for home, the destination and the mode. Nil unless SURE or the words
name a way to travel: \"how long to boil an egg\" is no trip."
  (let ((ws (split-words text))
        (mode nil))
    (loop for (tail . m) in *modes*
          when (ends-with ws tail)
          do (setf ws (butlast ws (length tail)) mode m sure t)
          (return))
    (loop for tail in '(("leave") ("go") ("depart") ("leaving") ("going") ("from" "here")
                        ("from" "home"))
          when (ends-with ws tail) do (setf ws (butlast ws (length tail))))
    (when (equal (first ws) "to")
      (let ((verb (assoc (second ws) *travel-verbs* :test #'equal)))
        (cond ((null verb) nil)
              ((member (third ws) '("to" "from") :test #'equal)
               (setf ws (cddr ws) mode (or mode (cdr verb)) sure t))
              ;; "to get downtown by bus"
              (t (setf ws (cons "to" (cddr ws)))))))
    (flet ((trip (origin dest)
             (and sure dest (<= (length dest) 6) (<= (length origin) 6)
                  (not (and (null (cdr dest))
                            (member (first dest) *not-destinations* :test #'equal)))
                  (list (and origin (not (member origin '(("here") ("home")) :test #'equal))
                             (unwords origin))
                        (unwords dest) (or mode default)))))
      (let ((to (position "to" ws :test #'equal :start (min 1 (length ws))))
            (from (position "from" ws :test #'equal :start (min 1 (length ws)))))
        (cond ((and (equal (first ws) "from") to (> to 1))
               (setf sure t)
               (trip (subseq ws 1 to) (nthcdr (1+ to) ws)))
              ((and (equal (first ws) "to") from (> from 1))
               (setf sure t)
               (trip (nthcdr (1+ from) ws) (subseq ws 1 from)))
              ((equal (first ws) "to") (trip nil (rest ws)))
              (t nil))))))

(defn parse-route ((raw string)) (or null hash-table)
  "What `gmaps-cli route --json` printed, nil when that is no route."
  (let ((v (parse-json raw)))
    (and (hash-table-p v) (integerp (json-get v "duration_seconds"))
         (integerp (json-get v "distance_meters")) v)))

(defn steps ((trip hash-table)) list
  (let ((steps (json-get trip "steps")))
    (and (vectorp steps) (not (stringp steps)) (coerce steps 'list))))

(defn rides ((trip hash-table)) list
  "The legs on a vehicle, as their transit details."
  (loop for s in (steps trip)
        for ride = (json-get s "transit")
        when (and (hash-table-p ride) (stringp (json-get ride "line"))) collect ride))

(defn text-at ((v (or null json)) (key string)) (or null string)
  (let ((s (json-get v key))) (and (stringp s) (plusp (length s)) s)))

(defn local-clock ((iso string)) (or null string)
  "\"2026-11-12T22:46:31Z\" as \"11:46 PM\" here, nil when date fails."
  (let ((hm (output "date" "-d" iso "+%H %M")))
    (and hm (cl-ppcre:register-groups-bind ((#'parse-integer h m)) ("^(\\d+) (\\d+)$" hm)
                                           (format-hm (+ (* h 3600) (* m 60)))))))

(defn spoken-distance ((metres (integer 0))) string
  (cond ((< metres 1000) (format nil "~d metres" (* 10 (round metres 10))))
        ((< metres 10000) (format nil "~,1f kilometres" (/ metres 1000)))
        (t (format nil "~d kilometres" (round metres 1000)))))

(defn place-words ((place string)) string
  "\"berlin\" -> \"Berlin\"; \"the airport\" and names from the map stay."
  (if (or (uiop:string-prefix-p "the " place) (some #'upper-case-p place))
      place
      (string-capitalize place)))

(defn sentence-case ((s string)) string
  (string-upcase s :end (min 1 (length s))))

(defn by ((mode string)) string
  (cond ((string= mode "driving") "by car")
        ((string= mode "walking") "on foot")
        ((string= mode "bicycling") "by bike")
        (t "by public transport")))

(defn trip-speech ((trip hash-table) (dest string) (mode string) (clock function)) string
  "CLOCK turns a ride's UTC time into what to say, or nil."
  (let ((rides (rides trip))
        (duration (spoken-duration (* 60 (max 1 (floor (json-get trip "duration_seconds") 60))))))
    (if (and rides (string= mode "transit"))
        (let ((first (first rides)) (changes (1- (length rides))))
          (format nil "Take the ~a~@[ from ~a~]~@[ at ~a~]~@[, then ~{the ~a~^, ~}~]. ~a, ~a ~
door to door."
                  (json-get first "line") (text-at first "departure_stop")
                  (let ((at (text-at first "departure_time"))) (and at (funcall clock at)))
                  (mapcar (lambda (r) (json-get r "line")) (take 2 (rest rides)))
                  (case changes (0 "No changes") (1 "One change")
                        (otherwise (format nil "~d changes" changes)))
                  duration))
        (format nil "~a is ~a away, about ~a ~a." (sentence-case (place-words dest))
                (spoken-distance (json-get trip "distance_meters")) duration (by mode)))))

(defn vehicle-glyph ((vehicle (or null string))) string
  (if (and vehicle (search "BUS" vehicle)) "bus" "train"))

(defn first-line ((s string)) string
  (subseq s 0 (or (position #\Newline s) (length s))))

(defn trip-items ((trip hash-table) (mode string) (clock function)) list
  "The panel's rows. On public transport the turns of a walk fold into one
row; otherwise the first turns are enough to recognise the way."
  (if (and (rides trip) (string= mode "transit"))
      (let (items (walked 0))
        (flet ((walk ()
                 (when (plusp walked)
                   (push (obj "glyph" "walk" "title" "Walk" "meta" (format nil "~d min" walked))
                         items)
                   (setf walked 0)))
               (at (ride key) (let ((iso (text-at ride key))) (and iso (funcall clock iso)))))
          (dolist (s (steps trip))
            (let ((ride (json-get s "transit")))
              (cond ((and (hash-table-p ride) (text-at ride "line"))
                     (walk)
                     (push (obj "glyph" (vehicle-glyph (text-at ride "vehicle"))
                                "title" (text-at ride "line")
                                "detail" (format nil "~@[~a~]~@[ → ~a~]"
                                                 (text-at ride "departure_stop")
                                                 (text-at ride "arrival_stop"))
                                "meta" (format nil "~@[~a~]~@[ – ~a~]" (at ride "departure_time")
                                               (at ride "arrival_time")))
                           items))
                    (t (incf walked (or (parse-integer (or (text-at s "duration") "")
                                                       :junk-allowed t)
                                        0))))))
          (walk))
        (nreverse items))
      (loop for s in (take 8 (steps trip))
            collect (obj "glyph" "arrow-right"
                         "title" (first-line (or (text-at s "instruction") ""))
                         "meta" (format nil "~@[~a~]~@[ · ~a~]" (text-at s "distance")
                                        (text-at s "duration"))))))

(defn mode-glyph ((mode string)) string
  (cond ((string= mode "driving") "car")
        ((string= mode "walking") "walk")
        ((string= mode "bicycling") "bike")
        (t "train")))

(defn trip-summary ((trip hash-table) (mode string)) string
  (format nil "~@[~a · ~]~@[~a · ~]~a" (text-at trip "duration") (text-at trip "distance")
          (by mode)))

(defn trip-title ((dest string)) string (format nil "To ~a" (place-words dest)))

(defn trip-view ((trip hash-table) (dest string) (mode string) (clock function)) hash-table
  (obj "kind" "timeline" "icon" (mode-glyph mode)
       "title" (trip-title dest)
       "subtitle" (trip-summary trip mode)
       "url" (text-at trip "url")
       "items" (coerce (trip-items trip mode clock) 'vector)))

(defreplies *travel-filler*
  "Checking the map." "Let me look at the map." "One sec, asking Google where things are.")

(defreplies *no-route*
  "I can't find a way to ~a. Are you sure it exists?"
  "No route to ~a. Maybe stay home."
  "The map shrugged at ~a. Try another name.")

(defn homeless () reply
  (live "Travel" (format nil "Put your address on the first line of ~a" (home-path))
        "I don't know where home is. Write your address into the home file in my config folder."))

(defn parse-places ((raw string)) list
  "What `gmaps-cli nearby --json` printed, the places with a name."
  (let ((places (json-get (parse-json raw) "places")))
    (and (vectorp places) (not (stringp places))
         (remove-if-not (lambda (p) (text-at p "name")) (coerce places 'list)))))

(defn landmark ((dest string) (start string)) (values string (or null string))
  "\"the airport\" is the one near START, not whichever the geocoder likes:
its address to route to and its name, or DEST as it was."
  (let* ((what (and (uiop:string-prefix-p "the " dest) (subseq dest 4)))
         (raw (and what (run "gmaps-cli" "nearby" what "--location" start "--limit" "1"
                             "--json")))
         (place (and raw (first (parse-places raw))))
         (address (text-at place "address")))
    (if address
        (values (format nil "~a, ~a" (text-at place "name") address) (text-at place "name"))
        (values dest nil))))

(defn route ((origin (or null string)) (dest string) (mode string)) reply
  (let ((start (or origin (home))))
    (if (null start)
        (homeless)
        (multiple-value-bind (target name) (landmark dest start)
          (let* ((raw (run "gmaps-cli" "route" start target "--mode" mode "--json"))
                 (trip (and raw (parse-route raw)))
                 (dest (or name dest)))
            (cond ((null trip) (live "Travel" dest (pick *no-route* (place-words dest))))
                  (t (viewed (trip-view trip dest mode #'local-clock) (trip-title dest)
                             (trip-summary trip mode)
                             (trip-speech trip dest mode #'local-clock)))))))))

(defn street ((address string)) string
  "\"Lindwurmstraße 97, 80337 München, Germany\" -> \"Lindwurmstraße 97\"."
  (trim (subseq address 0 (or (position #\, address) (length address)))))

(defn places-speech ((places list)) string
  (format nil "~a~@[, ~a~].~@[ Also ~{~a~^ and ~}.~]" (text-at (first places) "name")
          (let ((address (text-at (first places) "address"))) (and address (street address)))
          (mapcar (lambda (p) (text-at p "name")) (take 2 (rest places)))))

(defn places-title ((what string) (near (or null string))) string
  (format nil "~a ~:[nearby~;near ~:*~a~]" (sentence-case what)
          (and near (string-capitalize near))))

(defn places-view ((places list) (what string) (near (or null string))) hash-table
  (obj "kind" "list" "icon" "map-pin" "title" (places-title what near)
       "items" (map 'vector
                    (lambda (p)
                      (obj "glyph" "map-pin" "title" (text-at p "name")
                           "detail" (text-at p "address")
                           "meta" (let ((rating (json-get p "rating")))
                                    (and (realp rating) (format nil "★ ~,1f" rating)))
                           "url" (text-at p "url")))
                    places)))

(defreplies *no-places*
  "I found no ~a around here. Tragic."
  "No ~a nearby. Time to move."
  "Nothing. Not a single ~a. What a neighbourhood.")

(defparameter *not-near*
  '("eat" "stay" "park" "buy" "get" "drink" "swim" "sleep" "go" "shop" "work" "sit" "live")
  "\"The nearest place to eat\" names no second place.")

(defn split-near ((text string)) list
  "\"gas station to glendale\" -> (\"gas station\" \"glendale\"): what to look
for and the place to look around, nil for home."
  (let* ((ws (split-words text))
         (at (position-if (lambda (w) (member w '("to" "near" "in") :test #'equal)) ws
                          :start (min 1 (length ws))))
         (near (and at (nthcdr (1+ at) ws))))
    (if (and near (not (and (null (cdr near)) (member (first near) *not-near* :test #'equal))))
        (list (unwords (subseq ws 0 at)) (unwords near))
        (list text nil))))

(defn nearby ((what string) (near (or null string))) reply
  (let ((start (or near (home))))
    (if (null start)
        (homeless)
        (let* ((raw (run "gmaps-cli" "nearby" what "--location" start "--limit" "5" "--json"))
               (places (and raw (parse-places raw))))
          (cond ((null places) (live "Nearby" what (pick *no-places* what)))
                (t (viewed (places-view places what near) (places-title what near)
                           (format nil "~{~a~^~%~}"
                                   (mapcar (lambda (p) (text-at p "name")) places))
                           (places-speech places))))))))

(defcommand travel-route
  (:join "!(when's|when is|what's|what is|when does|when goes) the next"
         "(train|bus|tram|subway|connection) {text}")
  "!next (train|bus|tram|subway|connection) {text}"
  "!how do i get {text}"
  "![give me|show me|get] (directions|route|navigate|take me) {text}"
  "!how long [does it take|is it|will it take|would it take] [me|us] {text}"
  "!how far is it {text}"
  "!how far [away] is {text} from (here|home)"
  :kind "Route"
  :named t
  :slots ((origin (coalton:Optional coalton:String)) (dest coalton:String) (mode coalton:String))
  :parse (let ((text (slot "text")))
           (cond ((null text) nil)
                 ((search "how far [away]" template) (list nil text "driving"))
                 ((search "how far" template) (parse-trip text "driving" :sure t))
                 ((search "how long" template) (parse-trip text "transit"))
                 (t (parse-trip text "transit" :sure t))))
  :filler *travel-filler*
  :examples ("when is the next train to laim" "how long does it take to drive to berlin")
  (route origin dest mode))

(defcommand travel-nearby
  (:join "!(find|show|where is|where's|is there|are there) [me] [a|an|the|any|some]"
         "(nearest|closest|nearby) {text}")
  (:join "!(find|show|where is|where's|is there|are there) [me] [a|an|the|any|some] {text}"
         "(nearby|near me|near here|close by|around here|in the area|near home)")
  "!(nearest|closest) {text}"
  :kind "Nearby"
  :named t
  :slots ((what coalton:String) (near (coalton:Optional coalton:String)))
  :parse (let ((text (slot "text"))) (and text (split-near text)))
  :filler *travel-filler*
  :examples ("find a pharmacy nearby" "where is the nearest supermarket")
  (nearby what near))

(defskill travel)
