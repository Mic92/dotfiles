;;; Calendar and meetings: read with khal's JSON output, created with
;;; calendar-cli, which syncs right away.

(in-package #:va)

(in-skill meeting)

(defn meeting-hours ((c va.types:Clock)) (integer 0)
  "\"at 3\" for a meeting means 3 pm, meetings at 3 am are rare."
  (let ((secs (va.types:clock-secs c)))
    (if (and (va.types:clock-twelve-hour c) (< secs (* 7 3600))) (+ secs (* 12 3600)) secs)))

(defn split-day ((title string)) (values string (or null (integer 0)))
  "The greedy {text} slot may swallow an unmarked day, as in \"with anna
tomorrow\" or \"with anna on friday\"."
  (let ((ws (split-words title)))
    (loop for n from (min 4 (1- (length ws))) downto 1
          for d = (days-from-today (last ws n))
          when d return (values (unwords (butlast ws n)) d)
          finally (return (values title nil)))))

(defn create-fields ((text (or null string)) (day (or null (integer 0) va.types:Date))
                     (clock (or null va.types:Clock)) (prefix string))
    list
  "Create's fields, or nil. A day in the title counts when none was said."
  (when (and text clock)
    (multiple-value-bind (title said) (split-day (format nil "~a~a" prefix text))
      (let ((day (if day (day-offset day) (or said 0))))
        (and day (list title day (meeting-hours clock) nil))))))

(defparameter *video-hosts*
  '("meet.google.com" "zoom.us" "teams.microsoft.com" "teams.live.com" "meet.jit.si"
    "whereby.com" "webex.com")
  "Where a link in an event joins its video call.")

(defn video-link ((fields list)) (or null string)
  (loop for field in fields
        thereis (loop for w in (split-words field)
                      for url = (string-trim "<>()\",;" w)
                      thereis (and (uiop:string-prefix-p "https://" url)
                                   (some (lambda (h) (search h url)) *video-hosts*)
                                   url))))

(defstruct (event (:constructor make-event (start end title link all-day)))
  "A calendar event. Start and end are \"YYYY-MM-DD HH:MM\", or a bare date
for all-day events."
  (start "" :type string) (end "" :type string) (title "" :type string)
  (link nil :type (or null string)) (all-day nil :type boolean))

(defn parse-events ((raw string)) list
  "khal prints one JSON array per day."
  (flet ((field (e k) (let ((v (json-get e k))) (if (stringp v) v ""))))
    (let ((events (loop for line in (split-lines raw)
                        for day = (parse-json line)
                        when (and (vectorp day) (not (stringp day)))
                        append (loop for e across day
                                     for title = (trim (field e "title"))
                                     for start = (field e "start-long")
                                     when (and (plusp (length title)) (plusp (length start)))
                                     collect (make-event
                                              start (field e "end-long") title
                                              (video-link (list (field e "location")
                                                                (field e "url")
                                                                (field e "description")))
                                              (string= (field e "all-day") "True"))))))
      (remove-duplicates (sort events #'string< :key #'event-start) :test #'equalp))))

(defvar *calendar* nil "The one khal calendar to read, or nil for all of them.")

(defn khal (&rest (range string)) (values list boolean)
  "The events in RANGE, and whether khal answered at all."
  (let ((raw (apply #'run "khal" "list" "--day-format" ""
                    (append (loop for f in '("start-long" "end-long" "title" "location" "url"
                                             "description" "all-day")
                                  append (list "--json" f))
                            (and *calendar* (list "-a" *calendar*))
                            range))))
    (values (and raw (parse-events raw)) (and raw t))))

(defn minutes ((s string)) (or null integer)
  "Minutes since the epoch for \"YYYY-MM-DD HH:MM\" or a bare date."
  (cl-ppcre:register-groups-bind ((#'parse-integer y m d) h mi)
                                 ("^\\s*(\\d+)-(\\d+)-(\\d+)(?: (\\d+):(\\d+))?" s)
                                 (and (<= 1 m 12) (<= 1 d 31)
                                      (+ (* (day-number y m d) 1440)
                                         (* 60 (if h (parse-integer h) 0))
                                         (if mi (parse-integer mi) 0)))))

(defn date-ahead ((days (integer 0))) (or null string)
  (let ((s (run "date" "-d" (format nil "+~d days" days) "+%F"))) (and s (trim s))))

(defn now-stamp () (or null string)
  (let ((s (run "date" "+%F %H:%M"))) (and s (trim s))))

(defn spoken-mins ((m integer)) string
  (cond ((<= m 0) "now")
        ((= m 1) "one minute")
        ((< m 60) (format nil "~d minutes" m))
        ((= m 60) "an hour")
        ((< m 120) (format nil "an hour and ~d minutes" (- m 60)))
        ((< m 1440) (format nil "~d hours" (floor (+ m 30) 60)))
        (t (format nil "~d days" (floor (+ m 720) 1440)))))

(defn event-hm ((start string)) string
  "\"2025-10-22 14:00\" -> \"2 PM\"."
  (or (cl-ppcre:register-groups-bind ((#'parse-integer h m)) (" (\\d+):(\\d+)" start)
                                     (format-hm (+ (* h 3600) (* m 60))))
      ""))

(defn day-name ((day (integer 0))) string
  "\"today\", \"on Friday\", \"on Sunday, March 1\"."
  (let ((n (+ (context-today *context*) day)))
    (cond ((= day 0) "today")
          ((= day 1) "tomorrow")
          ((<= day 6) (format nil "on ~a" (subseq (spoken-day n) 0 (position #\, (spoken-day n)))))
          (t (format nil "on ~a" (spoken-day n))))))

(defreplies *calendar-broke*
  "I can't read your calendar. Khal is sulking."
  "Your calendar is locked. Maybe it's hiding something from me."
  "Calendar's not talking to me. We had a fight.")

(defn current-or-next ((events list) (now string) &key (all-day boolean nil))
    (values (or null event) integer)
  "The meeting in progress, or else the next one, and minutes until it.
All-day events count with ALL-DAY; khal ends them on their last day."
  (let ((n (minutes now)))
    (dolist (e events (values nil 0))
      (let ((start (minutes (event-start e)))
            (end (let ((end (minutes (event-end e))))
                   (if (and end (event-all-day e)) (+ end 1440) end))))
        (when (and n start (or all-day (not (event-all-day e)))
                   (or (> start n) (and end (> end n))))
          (return (values e (- start n))))))))

(defreplies *nothing-ahead*
  "Nothing in the next week. Are you on holiday, or just unpopular?"
  "No meetings ahead. Bliss." "Nothing scheduled. Did everyone finally give up on you?")
(defreplies *meeting-now*
  "You're in ~a right now! Why are you talking to me?" "~a. That's now. Go, go, go!")

(defn next-speech ((e event) (mins integer) (what string)) string
  (format nil "~a~:[~; Say join my meeting.~]"
          (cond ((<= mins 0) (pick *meeting-now* (event-title e)))
                ((<= mins 5) (format nil "~a in ~a! Hurry up, gorgeous." (event-title e)
                                     (spoken-mins mins)))
                ((<= mins 60) (format nil "~a in ~a. Time to grab a Mate first." (event-title e)
                                      (spoken-mins mins)))
                (t (format nil "Next up: ~a. That's in ~a. Relax." what (spoken-mins mins))))
          (and (event-link e) (<= mins 10))))

(defn event-day-name ((e event)) (or null string)
  "\"tomorrow\" or \"on Friday\" for when E starts."
  (let ((start (minutes (subseq (event-start e) 0 (min 10 (length (event-start e)))))))
    (and start (>= (floor start 1440) (context-today *context*))
         (day-name (- (floor start 1440) (context-today *context*))))))

(defn next-meeting () reply
  (multiple-value-bind (events ok) (khal "now" "7d")
    (let ((now (now-stamp)))
      (multiple-value-bind (e mins) (if now (current-or-next events now) (values nil 0))
        (cond ((not (and ok now)) (live "Calendar" "" (pick *calendar-broke*)))
              ((null e) (live "Calendar" "" (pick *nothing-ahead*)))
              (t (let ((what (format nil "~a ~@[~a ~]at ~a" (event-title e)
                                     (and (> mins 720) (event-day-name e))
                                     (event-hm (event-start e)))))
                   (live "Next meeting" what (next-speech e mins what)))))))))

(defreplies *joining*
  "Opening ~a. Fix your hair." "Joining ~a. Camera off if you're in pyjamas."
  "Joining ~a. Mute yourself, I've heard you chew." "~a. Go get them.")
(defreplies *no-meeting*
  "There's no meeting to join. Free as a bird."
  "No meeting. Nobody wants to see your face today. Lucky you.")

(defn join-meeting () reply
  (multiple-value-bind (events ok) (khal "now" "1d")
    (let ((now (now-stamp)))
      (multiple-value-bind (e mins) (if now (current-or-next events now) (values nil 0))
        (cond ((not (and ok now)) (live "Calendar" "" (pick *calendar-broke*)))
              ((null e) (live "Join meeting" "" (pick *no-meeting*)))
              ((> mins 15)
               (live "Join meeting" (event-title e)
                     (format nil "Your next meeting, ~a, is in ~a. Too early. Nobody likes the ~
early bird." (event-title e) (spoken-mins mins))))
              ((null (event-link e))
               (live "Join meeting" (event-title e)
                     (format nil "~a has no video link. In person? How retro." (event-title e))))
              (t (launch "xdg-open" (event-link e))
                 (live "Join meeting" (event-title e)
                       (if (> mins 2)
                           (format nil "Opening ~a. You're ~a early. Teacher's pet."
                                   (event-title e) (spoken-mins mins))
                           (pick *joining* (event-title e))))))))))

(defn in-part-p ((e event) (date string) (part (integer 0 7))) boolean
  "Whether E starts in the 3-hour forecast slot PART of DATE."
  (let ((s (minutes (event-start e))) (d (minutes date))
        (from (* part 180)) (to (+ (* (1+ part) 180) (if (= part 7) 60 0))))
    (and s d (<= from (- s d)) (< (- s d) to))))

(defreplies *free-then*
  "You're free then. Nap opportunity!" "Nothing then. Guard that time with your life."
  "Free. Block it before someone books it.")
(defreplies *empty-day*
  "Nothing on your calendar ~a. Free as a bird." "Your calendar is empty ~a. Netflix?"
  "Nothing ~a. Pretend to be busy anyway.")
(defreplies *empty-friday* "Nothing ~a and it's Friday. Leave early, I won't tell.")

(defn agenda-speech ((timed list) (when string) (part (or null (integer 0))) (day (integer 0)))
    string
  (flet ((at (e) (format nil "~a at ~a" (event-title e) (event-hm (event-start e)))))
    (let ((n (length timed)) (first (first timed)) (last (car (last timed))))
      (cond ((and (zerop n) part) (pick *free-then*))
            ((zerop n) (choose *empty-day* when
                               (and (= (weekday (context-today *context*)) 5) (= day 0))
                               *empty-friday*))
            ((= n 1) (format nil "Just one thing ~a: ~a." when (at first)))
            ((= n 2) (format nil "Two things ~a: ~a, and ~a." when (at first) (at last)))
            ((>= n 6) (format nil "~d meetings ~a. Starting with ~a, ending with ~a. Who hurt you?"
                              n when (at first) (at last)))
            (t (format nil "You have ~d things ~a. First up: ~a. Busy, busy." n when
                       (at first)))))))

(defn day-events ((date string) (part (or null (integer 0 7)))) (values list boolean)
  "DATE's events, within PART's 3-hour slot when given, and whether khal answered."
  (multiple-value-bind (events ok) (khal date "1d")
    (values (remove-if-not (lambda (e) (and (or (uiop:string-prefix-p date (event-start e))
                                                (event-all-day e))
                                            (or (null part) (in-part-p e date part))))
                           events)
            ok)))

(defn agenda-line ((e event)) string
  (if (event-all-day e)
      (format nil "all day ~a" (event-title e))
      (format nil "~a ~a" (event-hm (event-start e)) (event-title e))))

(defn agenda ((day (integer 0)) (part (or null (integer 0 7)))) reply
  (let ((date (date-ahead day)) (when (day-name day)))
    (multiple-value-bind (events ok) (if date (day-events date part) (values nil nil))
      (if (not ok)
          (live "Calendar" "" (pick *calendar-broke*))
          (live (format nil "Calendar ~a" when)
                (format nil "~{~a~^~%~}" (mapcar #'agenda-line events))
                (agenda-speech (remove-if #'event-all-day events) when part day))))))

(defparameter *shared-calendar* "Joerg+Shannan"
  "khal's name for the calendar shared with Shannan. Hers alone isn't synced
here, so questions about her plans are answered from this one.")

(defparameter *shared* "the calendar you share with Shannan"
  "How replies name it, so nobody takes its events for Shannan's alone.")

(defn shared-next-speech ((e (or null event)) (mins integer)) string
  (cond ((null e) (format nil "Nothing on ~a in the next month." *shared*))
        ((<= mins 0) (format nil "~a is on ~a right now." (event-title e) *shared*))
        (t (let ((day (and (or (event-all-day e) (> mins 720)) (event-day-name e)))
                 (hm (event-hm (event-start e))))
             (format nil "Next on ~a: ~a~@[ ~a~]~a." *shared* (event-title e) day
                     (cond ((event-all-day e) ", all day")
                           (day (format nil " at ~a" hm))
                           (t (format nil " at ~a, in ~a" hm (spoken-mins mins)))))))))

(defn shannan-next () reply
  (multiple-value-bind (events ok) (let ((*calendar* *shared-calendar*)) (khal "now" "30d"))
    (let ((now (now-stamp)))
      (if (not (and ok now))
          (live "Calendar" "" (pick *calendar-broke*))
          (multiple-value-bind (e mins) (current-or-next events now :all-day t)
            (live "Shared calendar" (if e (agenda-line e) "") (shared-next-speech e mins)))))))

(defn shared-day-speech ((events list) (when string)) string
  (if (null events)
      (format nil "Nothing on ~a ~a." *shared* when)
      (format nil "On ~a ~a: ~{~#[~;~a~;~a, and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}." *shared* when
              (loop for e in events
                    collect (if (event-all-day e)
                                (format nil "~a all day" (event-title e))
                                (format nil "~a at ~a" (event-title e)
                                        (event-hm (event-start e))))))))

(defn shannan-day ((day (integer 0))) reply
  (let ((date (date-ahead day)) (when (day-name day)))
    (multiple-value-bind (events ok)
        (if date (let ((*calendar* *shared-calendar*)) (day-events date nil)) (values nil nil))
      (if (not ok)
          (live "Calendar" "" (pick *calendar-broke*))
          (live (format nil "Shared calendar ~a" when)
                (format nil "~{~a~^~%~}" (mapcar #'agenda-line events))
                (shared-day-speech events when))))))

(defreplies *free-at* "You're free at ~a. Want me to book it?" "~a is free. Treasure it.")

(defn clash ((events list) (tm integer)) (or null event)
  "The timed event going on at TM, in minutes."
  (find-if (lambda (e)
             (let ((s (minutes (event-start e))) (end (minutes (event-end e))))
               (and (not (event-all-day e)) s end (<= s tm) (> end tm))))
           events))

(defn free-at ((day (integer 0)) (secs (integer 0))) reply
  (let* ((date (date-ahead day))
         (start (and date (minutes date)))
         (when (format nil "~a ~a" (format-hm secs) (day-name day))))
    (multiple-value-bind (events ok) (if start (khal date "1d") (values nil nil))
      (let ((e (clash events (+ (or start 0) (floor secs 60)))))
        (cond ((not ok) (live "Calendar" "" (pick *calendar-broke*)))
              (e (live "Calendar" (event-title e)
                       (format nil "No, at ~a you have ~a. Popular, aren't you?" when
                               (event-title e))))
              (t (live "Calendar" when (pick *free-at* when))))))))

(defreplies *meeting-added*
  "Done. ~a is in your calendar." "Booked: ~a. I'll remind you ten minutes before."
  "~a, booked. Try to show up on time for once." "~a. Added. Look at you, organised.")

(defn create-event ((title string) (day (integer 0)) (secs (integer 0)) (confirmed boolean))
    reply
  (let ((when (format nil "~a ~a" (day-name day) (format-hm secs)))
        (summary (string-capitalize title :end (min 1 (length title))))
        (date (and confirmed (date-ahead day))))
    (cond ((not confirmed)
           (reply "New meeting?" (format nil "~a, ~a" summary when)
                  (format nil "~a, ~a. Shall I add it?" summary when)
                  :live t
                  :question (confirm (command-action 'meeting-create title day secs t)
                                     (say-action (format nil "Okay, no ~a. Your calendar thanks ~
you." summary)))))
          ((null date) (live "Calendar" "" (pick *calendar-broke*)))
          ((run "calendar-cli" "new" summary "--start"
                (format nil "~a ~2,'0d:~2,'0d" date (floor secs 3600) (mod (floor secs 60) 60))
                "--timezone" (or (let ((tz (run "timedatectl" "show" "--property=Timezone"
                                                "--value")))
                                   (and tz (plusp (length (trim tz))) (trim tz)))
                                 "Europe/Berlin")
                "-d" "60" "--alarm" "10m")
           (live "Meeting added" (format nil "~a, ~a" summary when) (pick *meeting-added* summary)))
          (t (live "Calendar" summary
                   "Calendar CLI refused. Try again, or do it by hand like a caveman.")))))

(defreplies *ask-meeting-when*
  "When is ~a?" "~a. Which day and what time?"
  "Sure. When? Day and time, please, I'm not psychic.")

(defn ask-meeting-when ((title string) (day (or null (integer 0)))) function
  "\"At what time?\" or \"When?\" for a new event: \"at 3\", \"friday at 9\"."
  (lambda (text)
    (let* ((ws (normalize text))
           (at (position "at" ws :test #'string=))
           (day (cond ((and at (plusp at)) (days-from-today (subseq ws 0 at)))
                      (at (or day 0))
                      (t day)))
           (clock (and day (parse-clock (if at (nthcdr (1+ at) ws) ws)))))
      (and clock (command-action 'meeting-create title day (meeting-hours clock) nil)))))

(defn ask-meeting ((title string) (day (or null (integer 0)))) reply
  (reply "Calendar" title
         (if day (format nil "What time ~a?" (day-name day)) (pick *ask-meeting-when* title))
         :question (question :answer (ask-meeting-when title day))))

(defn refine-meeting ((previous action) (ws words)) (or null action)
  "\"and tomorrow?\", \"and in the afternoon?\", \"and at 4?\""
  (let* ((d (funcall (slot-parser "day") ws nil))
         (d (and (integerp d) (<= d 2) d)))
    (case (command-name (action-command previous))
      ((meeting-day meeting-day-asked meeting-when)
       (multiple-value-bind (pd pp) (day-part ws)
         (cond (d (command-action 'meeting-day d nil))
               (pd (command-action 'meeting-day pd pp)))))
      (meeting-free-at
       (destructuring-bind (day secs) (action-fields previous)
         (let ((c (parse-clock (strip-one ws "at"))))
           (cond (d (command-action 'meeting-free-at d secs))
                 (c (command-action 'meeting-free-at day (meeting-hours c)))))))
      (otherwise nil))))

(defparameter *calendar-changes*
  '("add" "added" "put" "schedule" "delete" "remove" "clear" "cancel" "erase" "take" "move"
    "change" "make" "create" "book" "leave" "rid" "need")
  "Verbs of questions that change the calendar instead of reading it.")

(defcommand meeting-next
  "!(what's|what is|when's|when is) my next (meeting|appointment|event|call)"
  "!(what's|what is) next [on my calendar]"
  "!(do i have|have i got|is there) a (meeting|call) (soon|now|coming up|right now)"
  "!how (long|much time) (until|till|before) my next (meeting|appointment|call)"
  "!(am i|are we) in a meeting [right now|now]"
  :kind "Next"
  :examples ("what's my next meeting" "am i in a meeting")
  (next-meeting))

(defcommand meeting-join
  "!(join|open|start|enter) [my|the|this] [next|current] (meeting|call|video call)"
  "!open [the] (meeting|meet|zoom|call) link"
  "!(take me to|let me into) [my|the] (meeting|call)"
  :kind "Join"
  :examples ("join my meeting" "open the zoom link")
  (join-meeting))

(defcommand meeting-shannan-next
  (:join "!(what's|what is|when's|when is) (shannan's|shannon's|shannans|shannons) next"
         "(meeting|appointment|event|call)")
  "!(what's|what is) next on (shannan's|shannon's|shannans|shannons) (calendar|agenda|schedule)"
  "!does (shannan|shannon) have a (meeting|call|appointment) (soon|now|coming up|right now)"
  :examples ("what's shannan's next meeting")
  (shannan-next))

(defcommand meeting-shannan-day
  (:join "!(what's|what is) on (shannan's|shannon's|shannans|shannons)"
         "(calendar|agenda|schedule) [{day}]")
  "!(what are|what're) (shannan's|shannon's|shannans|shannons) plans [{day}]"
  (:join "!what does (shannan|shannon) have [planned|scheduled|going on|on her calendar]"
         "[{day}]")
  (:join "!does (shannan|shannon) have (anything|something|any meetings|any appointments|any"
         "plans|plans) [planned|scheduled|going on] [{day}]")
  "!(what's|what is) (shannan|shannon) (doing|up to) [{day}]"
  "!is (shannan|shannon) (free|busy|away|around) [{day}]"
  :named t
  :slots ((day coalton:UFix))
  :parse (let ((day (slot "day"))) (let ((d (if day (day-offset day) 0))) (and d (list d))))
  :examples ("what's on shannan's calendar tomorrow" "is shannan free on friday")
  (shannan-day day))

(defcommand meeting-day
  "(what's|what is) on my (calendar|agenda|schedule) [{day}]"
  "what do i have [on] {day}"
  "(any|do i have) (meetings|appointments|events) {day}"
  "(any|do i have) (meetings|appointments|events)"
  "(how busy am i|how does my day look|what does my day look like) [{day}]"
  "!(calendar|agenda|schedule) [{day}]"
  "!what's my day like"
  "!(what's|what is) (happening|planned|going on) {day}"
  "!(what's|what is) (on|in) the (schedule|calendar|books) {day}"
  (:join "![tell me|let me know] (what's|what is) (on|showing on|currently on) my"
         "(calendar|schedule|agenda) {day}")
  (:join "!(what do i have|what have i got|what've i got) (scheduled|planned|going on|to"
         "do|set|on) {day} [on my calendar|that's on my calendar]")
  "!(what do i have|what have i got) (scheduled|planned|going on|to do) [on my calendar] {day}"
  (:join "!(do i have|have i got|have i scheduled|is there) (anything|something|any events|any"
         "meetings|any appointments) [scheduled|planned|to do|on my calendar|on the schedule|on"
         "the calendar] {day}")
  "!(anything|any events) on (the|my) (schedule|calendar) {day}"
  "!(how is|how's) my (calendar|schedule|day) [looking] {day}"
  "!is my (calendar|schedule|day) (free|clear|empty|busy) {day}"
  "!what (events|meetings|appointments) (are|do i have) on my calendar {day}"
  :kind "Day"
  :named t
  :slots ((day coalton:UFix) (part (coalton:Optional coalton:UFix)))
  :parse (let ((day (slot "day"))) (let ((d (if day (day-offset day) 0))) (and d (list d nil))))
  :refine #'refine-meeting
  :examples ("what's on my calendar today" "do i have any meetings tomorrow")
  (agenda day part))

;; Questions vary endlessly, but a calendar word and a date at the end ask
;; for that day. Changes start with a verb and are left out.
(defcommand meeting-day-asked
  (:join "!{text}"
         "(calendar|schedule|appointments|events|agenda|plans|planned|scheduled|happening|day)"
         "[like|look like|looking|open|free] [on|for] {day} [on my calendar|please]")
  :kind "Day"
  :named t
  :slots ((day coalton:UFix) (part (coalton:Optional coalton:UFix)))
  :parse (let ((text (slot "text")) (day (slot "day")))
           (and text (notany (lambda (w) (member w *calendar-changes* :test #'string=))
                             (split-words text))
                (let ((d (if day (day-offset day) 0))) (and d (list d nil)))))
  :refine #'refine-meeting
  :examples ("can you tell me how my calendar looks for friday")
  (agenda day part))

(defcommand meeting-when
  "(what's|what is) on my (calendar|agenda|schedule) {when}"
  "what do i have {when}"
  "(any|do i have) (meetings|appointments|events) {when}"
  "!am i (free|busy) {when}"
  :kind "Day"
  :named t
  :slots ((day coalton:UFix) (part (coalton:Optional coalton:UFix)))
  :parse (let ((w (slot "when"))) (and w (cdr w) (list (car w) (cdr w))))
  :refine #'refine-meeting
  :examples ("am i free this afternoon")
  (agenda day part))

(defcommand meeting-free-at
  "!am i (free|busy|available) [{day}] at {clock}"
  "!am i (free|busy|available) at {clock} {day}"
  "!(do i have|have i got) (a meeting|anything|something|a call) [{day}] at {clock}"
  "!(do i have|have i got) (a meeting|anything|something|a call) at {clock} {day}"
  "!(is|are) [there] (anything|a meeting) at {clock} [{day}]"
  :kind "FreeAt"
  :named t
  :slots ((day coalton:UFix) (secs coalton:UFix))
  :parse (let* ((day (slot "day")) (d (if day (day-offset day) 0))
                (clock (or (slot "clock") (slot "time"))))
           (and d clock (list d (meeting-hours clock))))
  :refine #'refine-meeting
  :examples ("am i free at 3" "do i have a meeting tomorrow at 10")
  (free-at day secs))

(defcommand meeting-create
  (:join "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|call) with {text}"
         "[{day}] at {clock}")
  (:join "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|call) with {text} at"
         "{clock} {day}")
  (:join "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|appointment|event|call)"
         "(about|for|called|named) {text} [{day}] at {clock}")
  (:join "!(schedule|create|add|book|set up|make|put in) [a|an] (meeting|appointment|event|call)"
         "(about|for|called|named) {text} at {clock} {day}")
  "!(schedule|create|add|book|put) {text} (in|on|into|to) [my] calendar [{day}] at {clock}"
  "!(schedule|create|add|book|put) {text} (in|on|into|to) [my] calendar at {clock} {day}"
  :kind "Create"
  :named t
  :slots ((title coalton:String) (day coalton:UFix) (secs coalton:UFix)
          (confirmed coalton:Boolean))
  :parse (create-fields (slot "text") (slot "day") (or (slot "clock") (slot "time"))
                        (if (search "with {text}" template) "Meeting with " ""))
  :examples ("schedule a meeting with anna tomorrow at 3" "put dentist in my calendar at 9")
  (create-event title day secs confirmed))

(defcommand meeting-ask
  (:join "![i need to|i want to|i must|i'd like to|please] (add|put|schedule) [a|an|my] {text}"
         "(to|on|in|into) [my|the] (calendar|schedule) {day}")
  (:join "![i need to|i want to|i must|i'd like to|please] (add|put|schedule) [a|an|my] {text}"
         "(to|on|in|into) [my|the] (calendar|schedule)")
  :kind "Ask"
  :named t
  :slots ((title coalton:String) (day (coalton:Optional coalton:UFix)))
  :parse (let ((text (slot "text")) (day (slot "day")))
           (when text
             (multiple-value-bind (title said) (split-day text)
               (list title (if day (day-offset day) said)))))
  :examples ("add the dentist to my calendar" "put a haircut on my calendar friday")
  (ask-meeting title day))

(defskill meeting)
