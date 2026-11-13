;;; Time zones: the time and date elsewhere.

(in-package #:va)

(in-skill calc :continued t)

(defparameter *fixed-offsets*
  '(("CET" . "Etc/GMT-1") ("CEST" . "Etc/GMT-2") ("WET" . "Etc/GMT") ("WEST" . "Etc/GMT-1")
    ("EET" . "Etc/GMT-2") ("EEST" . "Etc/GMT-3") ("BST" . "Etc/GMT-1") ("MSK" . "Etc/GMT-3")
    ("EST" . "Etc/GMT+5") ("EDT" . "Etc/GMT+4") ("CST" . "Etc/GMT+6") ("CDT" . "Etc/GMT+5")
    ("MST" . "Etc/GMT+7") ("MDT" . "Etc/GMT+6") ("PST" . "Etc/GMT+8") ("PDT" . "Etc/GMT+7")
    ("AKST" . "Etc/GMT+9") ("HST" . "Etc/GMT+10") ("JST" . "Etc/GMT-9") ("KST" . "Etc/GMT-9")
    ("SGT" . "Etc/GMT-8") ("HKT" . "Etc/GMT-8") ("AEST" . "Etc/GMT-10") ("AEDT" . "Etc/GMT-11")
    ("NZST" . "Etc/GMT-12") ("NZDT" . "Etc/GMT-13") ("IST" . "Asia/Kolkata"))
  "Abbreviations are fixed offsets: \"3 pm EST\" is UTC-5 even in summer.
Etc/GMT signs are inverted.")

(defparameter *zone-aliases*
  '(("America/New_York" "eastern" "nyc" "new york city" "east coast" "eastern time" "boston"
     "washington" "miami" "atlanta" "philadelphia" "tampa" "bangor" "maine" "hartford" "orlando"
     "florida")
    ("America/Los_Angeles" "pacific" "southern california" "la" "san francisco" "california"
     "seattle" "west coast" "pacific time" "silicon valley" "las vegas" "vegas" "reno" "nevada"
     "san diego" "sacramento" "portland" "oregon")
    ("America/Chicago" "central" "tx" "texas" "houston" "dallas" "austin" "central time" "missouri"
     "st louis" "kansas city")
    ("America/Denver" "mountain" "mountain time" "colorado" "utah" "salt lake city")
    ("America/Detroit" "michigan")
    ("America/Boise" "idaho")
    ("America/Anchorage" "alaska")
    ("America/Lima" "peruvian")
    ("America/Guayaquil" "ecuador" "quito")
    ("America/Jamaica" "kingston")
    ("Asia/Pyongyang" "north korea")
    ("America/Phoenix" "arizona")
    ("America/Vancouver" "british columbia")
    ("Asia/Ho_Chi_Minh" "hanoi" "saigon" "ho chi minh city")
    ("Europe/Madrid" "valencia" "spanish" "spain" "barcelona")
    ("Pacific/Honolulu" "hawaii" "hawaiian")
    ("America/Toronto" "montreal" "canada")
    ("America/Mexico_City" "mexico")
    ("America/Sao_Paulo" "brazil" "brasilia" "rio" "rio de janeiro")
    ("America/Argentina/Buenos_Aires" "argentina")
    ("Europe/London" "british" "greenwich" "greenwhich" "uk" "england" "the uk" "scotland"
     "edinburgh")
    ("Europe/Lisbon" "portugal")
    ("Europe/Paris" "french")
    ("Europe/Berlin" "german" "munich" "hamburg" "frankfurt")
    ("Europe/Amsterdam" "holland")
    ("Europe/Rome" "italian" "milan")
    ("Europe/Kyiv" "kiev")
    ("Europe/Moscow" "russia")
    ("Asia/Jerusalem" "tel aviv")
    ("Africa/Johannesburg" "cape town")
    ("Asia/Dubai" "uae" "abu dhabi")
    ("Asia/Kolkata" "indian" "delhi" "new delhi" "mumbai" "bangalore")
    ("Asia/Shanghai" "china" "chinese" "beijing")
    ("Asia/Tokyo" "japanese" "osaka")
    ("Asia/Seoul" "south korea")
    ("Australia/Sydney" "australia" "melbourne")
    ("UTC" "utc" "gmt")
    ("CET" "cet" "c e t" "central european time")
    ("CEST" "cest" "c e s t" "central european summer time")
    ("WET" "wet" "w e t" "western european time")
    ("WEST" "west" "w e s t" "western european summer time")
    ("EET" "eet" "e e t" "eastern european time")
    ("EEST" "eest" "e e s t" "eastern european summer time")
    ("BST" "bst" "b s t" "british summer time")
    ("MSK" "msk" "m s k" "moscow time")
    ("EST" "est" "e s t" "eastern standard time")
    ("EDT" "edt" "e d t" "eastern daylight time")
    ("CST" "cst" "c s t" "central standard time")
    ("CDT" "cdt" "c d t" "central daylight time")
    ("MST" "mst" "m s t" "mountain standard time")
    ("MDT" "mdt" "m d t" "mountain daylight time")
    ("PST" "pst" "p s t" "pacific standard time")
    ("PDT" "pdt" "p d t" "pacific daylight time")
    ("AKST" "akst" "a k s t" "alaska time")
    ("HST" "hst" "h s t" "hawaii standard time")
    ("JST" "jst" "j s t" "japan standard time")
    ("KST" "kst" "k s t" "korea standard time")
    ("SGT" "sgt" "s g t" "singapore time")
    ("HKT" "hkt" "h k t" "hong kong time")
    ("AEST" "aest" "a e s t" "australian eastern standard time")
    ("AEDT" "aedt" "a e d t" "australian eastern daylight time")
    ("NZST" "nzst" "n z s t" "new zealand standard time")
    ("NZDT" "nzdt" "n z d t" "new zealand daylight time")
    ("IST" "ist" "i s t" "indian standard time"))
  "Names tzdata does not know, for an IANA zone or one of *fixed-offsets*:
nicknames, states, demonyms and countries with several zones.")

(defvar *places* nil
  "Spoken place to zone, from tzdata and *zone-aliases*. Read on first use.")

(defn tzdata ((file string)) list
  "The rows of a tzdata table, split at tabs."
  (with-open-file (in (merge-pathnames file (uiop:ensure-directory-pathname
                                             (or (uiop:getenv "TZDIR") "/etc/zoneinfo"))))
    (loop for line = (read-line in nil) while line
          unless (uiop:string-prefix-p "#" line)
          collect (uiop:split-string line :separator '(#\Tab)))))

(defn zone-city ((iana string)) string
  "\"America/New_York\" -> \"New York\"."
  (substitute #\Space #\_ (subseq iana (1+ (or (position #\/ iana :from-end t) -1)))))

(defn country-zone ((rows list)) (or null string)
  "A country's zone: its only one, or the one tzdata says covers most of it."
  (let ((most (remove-if-not (lambda (r) (uiop:string-prefix-p "most" (string-downcase
                                                                       (or (fourth r) ""))))
                             rows)))
    (third (cond ((null (rest rows)) (first rows)) ((null (rest most)) (first most))))))

(defn read-places () hash-table
  (let ((places (make-hash-table :test #'equal)) (zones (tzdata "zone.tab")))
    (dolist (row (tzdata "iso3166.tab"))
      (destructuring-bind (code name) row
        (let ((zone (country-zone (remove code zones :key #'first :test-not #'string=))))
          (when zone
            (setf (gethash (string-downcase (cl-ppcre:regex-replace " \\(.*\\)" name "")) places)
                  zone)))))
    (dolist (row zones)
      (setf (gethash (string-downcase (zone-city (third row))) places) (third row)))
    (loop for (zone . names) in *zone-aliases*
          do (dolist (n names) (setf (gethash n places) zone)))
    places))

(defn find-zone ((ws words)) (or null string)
  "\"sydney, australia\" and \"houston, tx\": the city decides."
  (let* ((ws (strip-one (subseq ws 0 (position "," ws :test #'string=)) "the"))
         (tail (find-if (lambda (tail) (and (< (length tail) (length ws))
                                            (a:ends-with-subseq tail ws :test #'string=)))
                        '(("time" "zone") ("timezone") ("time"))))
         (name (unwords (butlast ws (length tail))))
         (name (if (uiop:string-suffix-p name "'s") (subseq name 0 (- (length name) 2)) name)))
    (values (gethash name (or *places* (setf *places* (read-places)))))))

(defn iana ((zone string)) string
  (or (cdr (assoc zone *fixed-offsets* :test #'string=)) zone))

(defslot ("zone" "origin") (ws next) (find-zone ws))

(defn tz ((zone (or null string))) string
  (if zone (format nil "tz(\"~a\")" (iana zone)) "local"))

(defn place-name ((zone (or null string))) string
  (cond ((null zone) "here")
        ((assoc zone *fixed-offsets* :test #'string=) zone)
        (t (zone-city zone))))

(defn hm-secs ((s string)) (or null (integer 0))
  (cl-ppcre:register-groups-bind ((#'parse-integer h m)) ("^(\\d+):(\\d+)$" s)
                                 (+ (* h 3600) (* m 60))))

(defn spoken-offset ((offset string)) (or null string)
  "\"+05:30\" -> \"UTC plus 5:30\", \"+00:00\" -> \"UTC\"."
  (cl-ppcre:register-groups-bind (sign h m) ("^([+-])0?(\\d+):(\\d+)$" offset)
                                 (let ((dir (if (string= sign "-") "minus" "plus")))
                                   (cond ((and (string= h "0") (string= m "00")) "UTC")
                                         ((string= m "00") (format nil "UTC ~a ~a" dir h))
                                         (t (format nil "UTC ~a ~a:~a" dir h m))))))

(defn zone-info ((out string)) (values (or null string) (or null string) (or null (integer 0)))
  "\"IST +05:30 01:40\" -> abbreviation, spoken offset, time there. Zones
without a name print the offset instead, like -03."
  (trivia:match (split-words out)
    ((list abbr offset hm)
     (let ((secs (hm-secs hm)) (spoken (spoken-offset offset)))
       (if (and secs spoken)
           (values (if (find (char abbr 0) "+-") "" abbr) spoken secs)
           (values nil nil nil))))
    (_ (values nil nil nil))))

(defreplies *zone-broke* "Time zones broke. Blame the British.")

(defreplies *zone-is*
  "~a."
  "~a. Now you know when to call."
  "~a. Write it on your hand."
  "~a. Time zones: humanity's worst invention after Mondays."
  "~a. Adjust your Club-Mate schedule accordingly."
  "~a. Long-distance thing going on? Tell me everything.")

(defn which-zone ((zone string)) reply
  (let ((out (numbat (format nil "format_datetime(\"%Z %:z %H:%M\", now() -> ~a)" (tz zone)))))
    (multiple-value-bind (abbr offset secs) (if out (zone-info out) (values nil nil nil))
      (if (null secs)
          (live "Time zone" "" (pick *zone-broke*))
          (let ((what (format nil "~a is on ~:[~a, ~;~*~]~a. It's ~a there" (place-name zone)
                              (member abbr '("" "UTC") :test #'string=) abbr offset
                              (format-hm secs))))
            (live "Time zone" what (pick *zone-is* what)))))))

(defn time-expr ((at (or null (integer 0))) (from (or null string)) (to (or null string)))
    string
  "numbat prints the start's day of year, then the result's day of year and
time, so we can tell tomorrow from today."
  (let* ((day (format nil "{format_datetime(\"%Y-%m-%d\", now() -> ~a)}" (tz from)))
         (start (if at
                    (format nil "datetime(\"~a ~2,'0d:~2,'0d~@[ ~a~]\")" day (floor at 3600)
                            (mod (floor at 60) 60) (and from (iana from)))
                    "now()")))
    (format nil "\"{format_datetime(\"%j\", ~a)} {format_datetime(\"%j %H:%M\", ~a -> ~a)}\""
            start start (tz to))))

(defn shifted ((out string)) (values (or null (integer 0)) (integer -1 1))
  "\"298 299 21:05\" -> 21:05, one day later."
  (trivia:match (split-words out)
    ((list a b hm)
     (let ((a (parse-integer a :junk-allowed t)) (b (parse-integer b :junk-allowed t))
           (secs (hm-secs hm)))
       (if (and a b secs)
           (let ((d (- b a))) (values secs (cond ((> d 1) -1) ((< d -1) 1) (t d))))
           (values nil 0))))
    (_ (values nil 0))))

(defreplies *zone-now*
  "~a."
  "~a. Hope they're awake."
  "~a. Don't call them if it's the middle of the night, monster."
  "~a. Time is relative anyway."
  "~a. Somewhere it's always Club-Mate o'clock."
  "~a. Jet lag not included."
  "~a. Planning a call or an escape?"
  "~a. Different place, same deadlines."
  "~a. Perfect time for a booty call? I'm not judging."
  "~a. I checked, the planet's still spinning."
  (:late "~a. Who are you texting at this hour?"
         "~a. At least someone over there is awake."
         "~a. Go to sleep, citizen of the world."))

(defn world-time ((at (or null (integer 0))) (from (or null string))
                  (to (or null string)))
    reply
  (let ((out (numbat (time-expr at from to))))
    (multiple-value-bind (secs shift) (if out (shifted out) (values nil 0))
      (if (null secs)
          (live "Time" "" (pick *zone-broke*))
          (let* ((there (format nil "~a~a ~a" (format-hm secs)
                                (case shift (1 " tomorrow") (-1 " yesterday") (otherwise ""))
                                (if to (format nil "in ~a" (place-name to)) "here")))
                 (what (if at
                           (format nil "~a ~a is ~a" (format-hm at)
                                   (if from (format nil "in ~a" (place-name from)) "here")
                                   there)
                           (format nil "It's ~a" there))))
            (live "Time" what (pick *zone-now* what)))))))

(defreplies *no-calendar* "My calendar fell off the wall.")

(defreplies *date-in*
  "~a."
  "That'll be ~a."
  "~a. Put it in your calendar, not in my memory."
  "~a. Assuming the world's still here."
  "~a. Plenty of time to procrastinate."
  "~a. Mark it with a Club-Mate."
  "~a. Planning ahead? Who are you and what did you do with my user?"
  "~a. Countdown's on.")

(defn date-in ((days (integer 1))) reply
  (let ((date (numbat (format nil "format_datetime(\"%A, %B %-d, %Y\", now() + ~d days)" days))))
    (if (null date)
        (live "Date" "" (pick *no-calendar*))
        (let ((what (if (= days 1)
                        (format nil "Tomorrow is ~a" date)
                        (format nil "In ~d days it's ~a" days date))))
          (live "Date" what (pick *date-in* what))))))

(defcommand calc-date-in
  (:join "!(what|which) (day|date) (will|would|is) it (be|gonna be) (in|after) {number} days"
         "[from now|from today]")
  (:join "!(what|which) (day|date) is it (gonna|going to) be (in|after) {number} days [from"
         "now|from today]")
  "!(what|which) (day|date) will it be {number} days from (now|today)"
  "!(what's|what is|what will be|what would be) the date [be] {number} days from (now|today)"
  "!what will the date be (in|after) {number} days [from now|from today]"
  "!what will (the date be|be the date) {number} days from (now|today)"
  "!(what's|what is|what will be) the date (in|after) {number} days [from now|from today]"
  "![what's|what is] the date [it will be] (in|after) {number} days [from now|from today]"
  "!the date it will be {number} days from (now|today)"
  "!what date it will be (in|after) {number} days [from now|from today]"
  "!(in|after) [another] {number} days [from now|from today] what (date|day) will it be"
  "!(in|after) [another] {number} days [from now|from today] what will (it|the date) be"
  "!(in|after) [another] {number} days [from now|from today] what will be the date"
  "!{number} days from (now|today) what will the date be"
  "!the date {number} days from (now|today)"
  "!when will it be in {number} days"
  :kind "DateIn"
  :slots ((days coalton:UFix))
  :parse (let ((n (slot "number")))
           (and n (= n (ftruncate n)) (<= 1 n 36500) (list (truncate n))))
  :repeatable t
  :examples ("what date will it be in 30 days")
  (date-in days))

(defcommand calc-tomorrow
  "!(what's|what is|what will be) [the] date tomorrow"
  "!(what's|what is) tomorrow's date"
  "!what will the date be tomorrow"
  "!what the date (is|will be) [for] tomorrow"
  "!what (day|date) is tomorrow"
  "!what (day|date) will it be tomorrow"
  "!what (day|date) is it tomorrow"
  "!what (is|'s) the date for tomorrow"
  "!what would tomorrow's date be"
  "![what's|what is] the date tomorrow [is what]"
  "!tomorrow's date"
  "!tomorrow what will the date be"
  "!what date it is tomorrow"
  :kind "DateIn"
  :slots ((days coalton:UFix))
  :parse (list 1)
  :examples ("what's the date tomorrow")
  (date-in days))

(defcommand calc-zone
  (:join "![tell me|give me|i need|i need to know|can you tell me|do you know|i would like to"
         "know|i'd like to know] [what's|what is] [the] (timezone|time zone) [now|currently]"
         "(of|for|in|where) {zone} [is]")
  (:join "!(what|which) (timezone|time zone) (is|does|do) {zone} [use|in|located"
         "in|located|reside in|be in]")
  "!(in which|in what) (timezone|time zone) (is|does) {zone} [located|reside|lie]"
  "!(what|which) (timezone|time zone) {zone} (is in|uses|use|is located in|lies in|has|falls in|is)"
  "!{zone} is in (what|which) (timezone|time zone)"
  "![what's|what is] the (timezone|time zone) [that] {zone} (is in|uses)"
  "!(what|which) (timezone|time zone) (does|do) {zone} (have|use|fall in|belong to)"
  "!in {zone} (what's|what is) the (timezone|time zone)"
  "!{zone} (timezone|time zone)"
  (:join "![what's|what is|tell me|can you tell me|do you know|i need|i need to know] {zone}"
         "(timezone|time zone) [is what]")
  :kind "Zone"
  :slots ((zone coalton:String))
  :examples ("what timezone is denver in")
  (which-zone zone))

(defcommand calc-time
  (:join "![tell me] (what time is it|what's the time|what is the time|what's the current"
         "time|what is the current time|the time) [now|right now] in {zone} [right now|now]")
  (:join "![tell me] (what time is it|what's the time|what is the time|what's the current time)"
         "[now|right now] in {zone} [right now|now]")
  (:join "![what's|what is] [the] [current|exact] time [right now|now] [over] (in|for) {zone}"
         "[right now|now]")
  (:join "!(what time is it|what time it is|what time would it be|what time is in|what time of"
         "day is it|how late is it) [now|right now|currently|exactly] [over] [there] in {zone}"
         "[right now|now]")
  "!in {zone} (what's the time|what time is it|what is the time)"
  "!(what's|what is|what time is) {time} [here|my time|local time] in {zone} [time]"
  "!(what's|what is|what time is) {time} (in|at) {origin} [time] (in|to|for) {zone} [time]"
  "!(what's|what is|what time is) {time} {origin} time (in|to|for) {zone} [time]"
  "!convert {time} [in] {origin} [time] (to|into) {zone} [time]"
  "!convert {time} [here|my time|local time] (to|into) {zone} [time]"
  "!(when|if) it's {time} in {origin} [then] what time is it [here|for me]"
  "!(when|if) it's {time} in {origin} [then] what time is it in {zone}"
  "!(when|if) it's {time} [here] [then] what time is it in {zone}"
  "!what time (will|would|is) it be in {zone} (at|when it's) {time} [here|my time]"
  :kind "Time"
  :named t
  :slots ((at (coalton:Optional coalton:UFix)) (from (coalton:Optional coalton:String))
          (to (coalton:Optional coalton:String)))
  :parse (let ((at (slot "time")) (from (slot "origin")) (to (slot "zone")))
           (and (or from to) (list (and at (va.types:clock-secs at)) from to)))
  :examples ("what time is it in tokyo" "what's 3 pm in london in new york")
  (world-time at from to))

(defskill calc)
