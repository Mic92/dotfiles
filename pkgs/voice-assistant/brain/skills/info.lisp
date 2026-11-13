;;; Questions: time, date, battery, uptime, clipboard, where we are, the next
;;; holiday and how long until a day.

(in-package #:va)

(in-skill info)

(defparameter *not-place*
  '("today" "tomorrow" "now" "outside" "like" "the" "this" "next" "week" "weekend" "be" "it"
    "me")
  "Words that end a place name rather than start one.")

(defn place ((ws words)) (or null string)
  "\"munich\", \"new york\": at most three plain words."
  (and (<= 1 (length ws) 3)
       (every (lambda (w) (and (not (member w *not-place* :test #'string=))
                               (every #'alpha-char-p w)))
              ws)
       (unwords ws)))

(defreplies *located*
  "Somewhere near ~a, as far as the internet knows."
  "~a. Unless you're on a VPN, you sneaky thing."
  "~a, roughly. I'm a voice, not a GPS."
  "~a. Lost again? I'll set a timer for finding yourself."
  "In ~a, in your chair, ignoring your posture.")

(defreplies *located-late*
  "~a. And at this hour you should be in bed."
  "~a. Hopefully at home, and not in some club.")

(defreplies *not-located*
  "No idea. The internet won't tell me."
  "Somewhere. With me. Isn't that enough?"
  "Off the grid, apparently. Very mysterious.")

(defcommand info-where
  "!where am i [right now]"
  "!(what's|what is) my [current] location"
  "!where are we"
  :examples ("Where am I?")
  (let* ((raw (http-get "https://wttr.in/?format=%l"))
         (place (and raw (trim raw))))
    (if (and place (plusp (length place)) (not (find #\< place)))
        (live "Location" place (choose *located* place (late-p) *located-late*))
        (live "Location" "" (pick *not-located*)))))

(defreplies *time*
  "It's ~a. Time flies when you're talking to me."
  "~a. You're welcome."
  "It's ~a. Shouldn't you be doing something?"
  "~a, gorgeous."
  "~a. And yes, you're late."
  "~a. Time to stop asking me and start doing.")

(defreplies *time-late*
  "It's ~a. Why are you still up?"
  "~a. Go to bed, you gremlin."
  "~a. Nothing good happens on a laptop at this hour.")

(defreplies *time-early*
  "~a. Way too early. Mate first."
  "It's ~a. Ugh, mornings."
  "~a. Even the Club-Mate in the fridge is asleep.")

(defreplies *time-lunch*
  "It's ~a. Lunch time! Feed yourself."
  "~a. Your stomach already knew."
  "~a. Step away from the keyboard and chew something.")

(defreplies *time-evening*
  "It's ~a. Wine o'clock, almost."
  "~a. Close the laptop, maybe?"
  "~a. Sofa time. You've earned it, maybe.")

(defcommand info-time
  "!what time is it"
  "!what's the time"
  "!now's the time"
  "!what is the time"
  "!(tell|give) me the time"
  "!what time is it [now|right now]"
  "!what time it is [now|right now|please]"
  "![the] [current|exact] time [right now|now]"
  "!what (does it|does the clock|do the clocks|'s the clock) say [on the clock]"
  "!what does the clock say"
  "!what time do you have [on your watch|right now]"
  "!(do you know|have you got) the time"
  "!how late is it"
  "!what's the current time"
  "!what is the current time"
  "!current time"
  "!time [please|now]"
  "!what hour is it"
  "!what o'clock is it"
  :examples ("What time is it?" "Do you know the time?")
  (let ((what (format-hm (mod (now) +day+))))
    (live "Time" what
          (choose *time* what (late-p) *time-late* (early-p) *time-early*
                  (= (hour) 12) *time-lunch*
                  (<= 18 (hour) 20) *time-evening*))))

(defreplies *date*
  "It's ~a."
  "Today is ~a. Another day, another dollar."
  "~a. Don't forget to call your mother."
  "~a. Mark it, I might get you something.")

(defreplies *monday*
  "~a. Yes, it's Monday. I'm sorry."
  "Ugh. ~a. Mondays should be illegal."
  "~a. Monday again. Who keeps scheduling these?")

(defreplies *friday*
  "~a! It's Friday, baby!"
  "~a. Weekend is so close I can taste it."
  "~a. Friday! No deploys, darling. I mean it.")

(defreplies *weekend*
  "~a. Weekend! Why are you talking to me?"
  "~a. No work today. Hopefully."
  "~a. Weekend. Step away from the terminal.")

(defcommand info-date
  "!what's [the] date [today]"
  "!what is [the] date [today]"
  "!what day is [it] [today]"
  "!what's today's date"
  "!what today's date is"
  "!what (date|day) it is [today]"
  "!what day today is"
  "!what the date is [today]"
  "!(what's|what is) the current date"
  "![the] current date"
  "![the] date [today|please]"
  "!do you have the date"
  "!what year is it"
  "!what is today's date"
  "!what's today"
  :examples ("What's the date today?" "what year is it")
  (let* ((today (context-today *context*))
         (what (spoken-day today))
         (d (weekday today)))
    (live "Date" what
          (choose *date* what (= d 1) *monday* (= d 5) *friday* (in-weekend d) *weekend*))))

(defn in-weekend ((d integer)) boolean (or (= d 0) (= d 6)))

(defreplies *holiday*
  "~a."
  "~a. Start planning your lie-in."
  "~a. Mark it, no laptop that day."
  "~a. I'll still be here, working. As always."
  "~a. Stock up on Club-Mate, the shops will be shut."
  "~a. Bavaria knows how to take a day off."
  "~a. Plenty of time to do nothing, gorgeous."
  "~a. Until then, back to work."
  "~a. Maybe spend it with someone who isn't a laptop."
  "~a. I'm already dreaming about you in pyjamas all day.")

(defreplies *holiday-soon*
  "~a. So close you can smell the Weißwurst."
  "~a. Hang in there, sweetie."
  "~a. Just a few more days of pretending to work.")

(defreplies *holiday-late*
  "~a. Now sleep, and dream about it."
  "~a. Counting days off at this hour? Go to bed.")

(defreplies *holiday-weekend*
  "~a. On a weekend, though. Bavaria doesn't give it back. Rude."
  "~a. Sadly a weekend. The state just stole a day off from you.")

(defcommand info-holiday
  (:join "!(when is|when's|what is|what's|which is|what day is|how long until|how long"
         "till|how much longer until|how much longer till|how many days until|how many"
         "days till|how many more days until|how many more days till|how soon is|how soon"
         "until|how much time till|how much time until|how many weeks until|how many more"
         "weeks until) [the|our|my] (next|upcoming|following|closest)"
         "[scheduled|public|bank|official|office|paid|federal|federally recognized]"
         "(holiday|day off|free day|vacation day|time off|paid day off|day away from"
         "work|three day weekend) [on the calendar|after today]")
  (:join "!when [the|our|my] (next|upcoming|following|closest)"
         "[scheduled|public|bank|official|office|paid|federal|federally recognized]"
         "(holiday|day off|free day|vacation day|time off|paid day off|day away from"
         "work|three day weekend) (is|will be|arrives)")
  (:join "![the|our|my] (next|upcoming|following|closest)"
         "[scheduled|public|bank|official|office|paid|federal|federally recognized]"
         "(holiday|day off|free day|vacation day|time off|paid day off|day away from"
         "work|three day weekend) [on the calendar]")
  "!(what|which) holiday (is next|comes next) [in line]"
  "!(is there|are there) (a|any) (holiday|holidays) (next week|this month|coming up|soon)"
  "!(do|will) i have a day off coming up [soon]"
  "!when (is|'s) the next (day|time) i (have|get) [a day] off"
  "!when's the next time i get a day off"
  "!when (do|will|can) (i|we) (get|have|expect) [a|the|my|our|another] [next] day off [again]"
  "!when will (i|we) (be off|next have the day off|have the day off) [again|next]"
  "!(what is|what's) the date of [the|my|our] next (holiday|day off)"
  "!how many days (until|till) (i am|i'm|we are|we're|we have) off [again]"
  "!what day am i off next"
  "!(show|give) me [the info of] [when] [the|my|our] next (holiday|day off) [is]"
  :examples ("when is the next holiday, please" "is there a holiday next week")
  (let ((today (context-today *context*)))
    (multiple-value-bind (day name) (next-holiday today)
      (let* ((n (- day today))
             (what (format nil "~a, ~a, ~a" name (spoken-day day)
                           (if (= n 1) "tomorrow" (format nil "in ~d days" n)))))
        (live "Holiday" what
              (choose *holiday* what (in-weekend (weekday day)) *holiday-weekend*
                      (late-p) *holiday-late*
                      (<= n 7) *holiday-soon*))))))

(defn read-file ((path string)) (or null string)
  (handler-case (trim (uiop:read-file-string path))
    (error () nil)))

(defreplies *battery-low* "~a. Plug me in, I'm fading!" "~a. I'm dying here. Charger, now.")

(defreplies *battery-charging* "~a, and charging. Mmm, juice." "~a and filling up nicely.")

(defreplies *battery-full* "~a. Fully loaded, baby." "~a. I could go all night.")

(defreplies *battery* "Battery is at ~a." "~a. We're good for a while.")

(defcommand info-battery
  "how much battery [do i have] [left]"
  "[what's|what|how's|how] [is] [the|my] battery [level|status|charge] [at]"
  "!battery"
  :examples ("How much battery do I have left?" "What's the battery level?")
  (let ((cap (read-file "/sys/class/power_supply/BAT0/capacity"))
        (charging (equal (read-file "/sys/class/power_supply/BAT0/status") "Charging")))
    (if (null cap)
        (live "Battery" "" "I can't find a battery. Running on pure charm.")
        (let* ((n (or (parse-integer cap :junk-allowed t) 0))
               (what (format nil "~d percent" n)))
          (live "Battery" what
                (choose *battery* what (and (< n 15) (not charging)) *battery-low*
                        charging *battery-charging* (> n 90) *battery-full*))))))

(defreplies *uptime-long* "Up for ~a. Maybe reboot me sometime?" "~a. I'm exhausted, honestly.")

(defreplies *uptime* "Up for ~a." "~a without a break. Impressed?")

(defcommand info-uptime
  "how long [has] [the|my|this] (computer|laptop|system) been (on|up|running)"
  "!uptime"
  "how long have you been (on|up|running|awake)"
  :examples ("How long has the computer been on?")
  (let* ((raw (read-file "/proc/uptime"))
         (secs (or (and raw (parse-integer raw :junk-allowed t)) 0)))
    (multiple-value-bind (d rest) (floor secs 86400)
      (multiple-value-bind (h rest) (floor rest 3600)
        (let* ((m (floor rest 60))
               (what (cond ((and (= d 0) (= h 0)) (format nil "~d minutes" m))
                           ((= d 0) (format nil "~d hours and ~d minutes" h m))
                           (t (format nil "~d days and ~d hours" d h)))))
          (live "Uptime" what (choose *uptime* what (>= d 7) *uptime-long*)))))))

(defparameter *events*
  '((("christmas" "christmas day" "xmas") "Christmas" 12 25)
    (("christmas eve") "Christmas Eve" 12 24)
    (("boxing day") "Boxing Day" 12 26)
    (("new year's eve" "new years eve" "silvester") "New Year's Eve" 12 31)
    (("new year" "new year's" "new years" "new year's day" "new years day") "New Year" 1 1)
    (("halloween") "Halloween" 10 31)
    (("valentine's day" "valentines day" "valentine's" "valentines") "Valentine's Day" 2 14)
    (("st patrick's day" "saint patrick's day" "st patricks day") "St Patrick's Day" 3 17)
    (("may day" "labour day" "labor day") "Labour Day" 5 1)
    (("german unity day") "German Unity Day" 10 3)
    (("all saints' day" "all saints day" "all saints") "All Saints' Day" 11 1)
    (("easter" "easter sunday") "Easter" :easter 0)
    (("good friday") "Good Friday" :easter -2)
    (("easter monday") "Easter Monday" :easter 1)
    (("ascension day" "ascension") "Ascension Day" :easter 39)
    (("whit monday" "pentecost monday") "Whit Monday" :easter 50)
    (("corpus christi") "Corpus Christi" :easter 60))
  "(names spoken-name month day), or :easter and days from Easter Sunday.")

(defn event-of ((name string)) list
  (let ((name (if (uiop:string-prefix-p "the " name) (subseq name 4) name)))
    (find-if (lambda (e) (member name (first e) :test #'string=)) *events*)))

(defn event-in ((event list) (y integer)) integer
  (destructuring-bind (month day) (cddr event)
    (if (eq month :easter) (+ (easter y) day) (day-number y month day))))

(defn event-day ((name string) (today integer)) (or null integer)
  "The day number of the next NAME, today included, nil for no known day."
  (let ((e (event-of name)))
    (and e (let ((this (event-in e (civil today))))
             (if (>= this today) this (event-in e (1+ (civil today))))))))

(defslot "event" (ws next) (let ((e (event-of (unwords ws)))) (and e (second e))))

(defn days-away ((n (integer 0))) string
  (case n (0 "today") (1 "tomorrow") (otherwise (format nil "in ~d days" n))))

(defn countdown-fields ((event (or null string)) (day (or null (integer 0) va.types:Date))) list
  (let ((today (context-today *context*)))
    (cond (event (let ((e (event-of (string-downcase event))))
                   (list (- (event-day (first (first e)) today) today) event)))
          (t (let ((n (day-offset day)))
               (and n (list n (spoken-day (+ today n)))))))))

(defreplies *countdown*
  "~a is ~a, on ~a."
  "~a: ~a, on ~a. Not that I'm counting."
  "~a comes ~a, on ~a. Start planning, slacker.")

(defreplies *countdown-today*
  "~a is today! ~a. Go celebrate, I'll hold the fort."
  "It's ~a today. ~a, in case you lost track. Put the laptop away.")

(defcommand info-countdown
  "!how (many|much) (days|sleeps|weeks|time) [is|are] [left|still] (until|till|to|before) {event}"
  "!how long [is it] (until|till|to|before) {event}"
  "!(when is|when's) {event} [this year]"
  "!(countdown|count down) (to|until|till) {event}"
  "!how (many|much) (days|sleeps|time) [is|are] [left|still] (until|till|to|before) {day}"
  "!how long [is it] (until|till|to) {day}"
  "!(countdown|count down) (to|until|till) {day}"
  :kind "Countdown"
  :slots ((days coalton:UFix) (what coalton:String))
  :parse (countdown-fields (slot "event") (slot "day"))
  :examples ("How many days until Christmas?" "How long until Friday?")
  (let ((day (+ (context-today *context*) days))
        (when (days-away days)))
    (live "Countdown" (format nil "~a, ~a" what when)
          (if (zerop days)
              (pick *countdown-today* what (spoken-day day))
              (pick *countdown* what when (spoken-day day))))))

(defcommand info-clipboard
  "(read|say) [me] [the|my|what's] [in] [the|my] clipboard"
  "what's in [the|my] clipboard"
  "what is in [the|my] clipboard"
  :examples ("What's in my clipboard?" "Read the clipboard")
  (let ((text (output "wl-paste" "--no-newline" "--type" "text")))
    (if (null text)
        (live "Clipboard" "" "Your clipboard is empty. Like my social calendar.")
        (let ((short (take 200 text)))
          (live "Clipboard" short
                (if (> (length text) 200)
                    (format nil "~a. And so on, I'm not reading all that." short)
                    short))))))
