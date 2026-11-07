;;; Questions: time, date, weather, battery, uptime, mail, clipboard, where
;;; we are and the next holiday.

(in-package #:va)

(in-skill info)

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

(defreplies *weather*
  "~a."
  "Outside: ~a. Inside: me, looking good."
  "~a. Now you know."
  "~a. Dress accordingly. Or don't, I can't see you."
  "~a. Perfect excuse to stay in.")

(defreplies *rainy*
  "~a. Take an umbrella, sweetie."
  "~a. Perfect weather for staying in with me."
  "~a. Wet. Like your git workflow.")

(defreplies *cold*
  "~a. Brr. Wear a jacket, I mean it."
  "~a. Cold. Cuddle weather."
  "~a. Bundle up, buttercup.")

(defreplies *hot*
  "~a. Hot. Almost as hot as me."
  "~a. Sunscreen, darling.")

(defreplies *no-weather*
  "I can't reach the weather. Look out the window?"
  "The weather service ghosted me."
  "The weather is a mystery today. Like your commit history.")

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

(defreplies *weather-filler*
  "Checking the sky."
  "Let me look outside. Metaphorically."
  "Sticking my head out the window. Brr.")

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

(defslot "place" (ws next) (place ws))

(defparameter *day-parts*
  '(("morning" . 3) ("noon" . 4) ("midday" . 4) ("afternoon" . 5) ("evening" . 6) ("night" . 7))
  "wttr.in's 3-hourly slots from midnight.")

(defn day-part ((ws words)) (values (or null (integer 0 2)) (or null (integer 0 7)))
  "\"tonight\", \"this evening\", \"tomorrow morning\", \"in the afternoon\": days
from today and the forecast slot."
  (flet ((part (w) (cdr (assoc w *day-parts* :test #'string=))))
    (multiple-value-bind (day p)
        (trivia:match ws
          ((list "tonight") (values 0 7))
          ((or (list (or "this" "in" "the") p) (list "in" "the" p)) (values 0 (part p)))
          ((or (list "tomorrow" p) (list "tomorrow" "in" "the" p)) (values 1 (part p)))
          ((or (list "day" "after" "tomorrow" p)
               (list "the" "day" "after" "tomorrow" p))
           (values 2 (part p))))
      (if p (values day p) (values nil nil)))))

(defslot "when" (ws next)
  (multiple-value-bind (day part) (day-part ws) (and day (cons day part))))

(defn part-name ((day (integer 0)) (part (integer 0))) string
  (let ((name (or (car (rassoc part *day-parts*)) "later")))
    (cond ((and (= day 0) (string= name "night")) "Tonight")
          ((= day 0) (format nil "This ~a" name))
          ((= day 1) (format nil "Tomorrow ~a" name))
          (t (format nil "The day after tomorrow in the ~a" name)))))

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

(defn weather-fields ((day (or null (integer 0) cons)) (place (or null string))) list
  "Fields from the day or day-part slot, nil beyond the day after tomorrow."
  (typecase day
    (null (list 0 place nil))
    (integer (and (<= day 2) (list day place nil)))
    (cons (and (<= (car day) 2) (list (car day) place (cdr day))))
    (otherwise nil)))

(defn refine-weather ((previous action) (ws words)) (or null action)
  "\"and tomorrow?\", \"and in the evening?\", \"and in berlin?\""
  (destructuring-bind (day place part) (action-fields previous)
    (let ((d (funcall (slot-parser "day") ws nil)))
      (multiple-value-bind (pd pp) (day-part ws)
        (cond ((and (integerp d) (<= d 2)) (command-action 'info-weather d place nil))
              (pd (command-action 'info-weather pd place pp))
              (t (let ((p (place ws))) (and p (command-action 'info-weather day p part)))))))))

(defcommand info-weather
  "what's the weather [like] [today|outside|now]"
  "what is the weather [like] [today|outside|now]"
  "how's the weather [today|outside]"
  "how is the weather [today|outside]"
  "(is|will) it (rain|raining|snow|snowing|cold|hot|warm|sunny) [today|outside]"
  "do i need (an|a) (umbrella|jacket|coat)"
  "!weather [today|now]"
  "!temperature [outside]"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {day}"
  "(what's|what is|how's|how is) the weather [like] (in|at) {place}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] (in|at) {place} {day}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {day} (in|at) {place}"
  "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) {day}"
  "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) (in|at) {place} [{day}]"
  "do i need (an|a) (umbrella|jacket|coat) {day}"
  "!weather {day}"
  "!weather (in|at|for) {place}"
  "!(weather|forecast) (in|at|for) {place} {day}"
  "!forecast [for] [{day}]"
  "what will the weather be [like] {day}"
  "what will the weather be [like] (in|at) {place}"
  "what will the weather be [like] {day} (in|at) {place}"
  "what will the weather be [like] (in|at) {place} {day}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {when}"
  "what will the weather be [like] {when}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] (in|at) {place} {when}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {when} (in|at) {place}"
  "(is|will) it (rain|snow|be cold|be hot|be warm|be sunny) {when}"
  "do i need (an|a) (umbrella|jacket|coat) {when}"
  "!weather {when}"
  :kind "Weather"
  :named t
  :slots ((day coalton:UFix) (place (coalton:Optional coalton:String))
          (part (coalton:Optional coalton:UFix)))
  :parse (weather-fields (or (slot "day") (slot "when")) (slot "place"))
  :refine #'refine-weather
  :filler *weather-filler*
  :examples ("What's the weather like today?" "How is the weather in New York tomorrow?")
  (weather day place part))

(defstruct (forecast (:constructor forecast (place cond temp feels high low rain)))
  "Current conditions for today, midday forecast otherwise, the forecast's
high and low and the chance of rain."
  (place "" :type string) (cond "" :type string) (temp 0 :type integer) (feels 0 :type integer)
  (high 0 :type integer) (low 0 :type integer) (rain 0 :type integer))

(defn first-value ((v (or null json)) (key string)) (or null string)
  "wttr.in wraps texts as [{\"value\": ...}]."
  (let ((s (json-get v key 0 "value"))) (and (stringp s) (trim s))))

(defn weather-source ((v json) (hourly vector) (day (integer 0)) (slot (or null json)))
    (or null json)
  "A day part's slot, the current conditions today, the midday forecast otherwise."
  (cond (slot)
        ((= day 0) (json-get v "current_condition" 0))
        (t (aref hourly (min 4 (1- (length hourly)))))))

(defn max-rain ((hours list)) (integer 0)
  (reduce #'max (mapcar (lambda (h) (or (json-int h "chanceofrain") 0)) hours) :initial-value 0))

(defn parse-weather ((json string) (day (integer 0)) (part (or null (integer 0))))
    (or null forecast)
  (let* ((v (parse-json json))
         (fc (json-get v "weather" day))
         (hourly (json-get fc "hourly")))
    (when (and (vectorp hourly) (plusp (length hourly)))
      (let* ((slot (and part (json-get hourly part)))
             (src (weather-source v hourly day slot))
             (desc (first-value src "weatherDesc"))
             (temp (json-int src "temp_C"))
             (feels (json-int src "FeelsLikeC"))
             (high (json-int fc "maxtempC"))
             (low (json-int fc "mintempC")))
        (and desc temp feels high low
             (forecast (or (first-value (json-get v "nearest_area" 0) "areaName") "")
                       desc temp feels high low
                       (max-rain (if slot (list slot) (coerce hourly 'list)))))))))

(defn weather-text ((w forecast) (day (integer 0)) (place (or null string))
                    (part (or null (integer 0))))
    string
  "What to say about W. \"In Munich: ...\" only when asked for somewhere else."
  (with-accessors ((desc forecast-cond) (temp forecast-temp) (feels forecast-feels)
                   (low forecast-low) (high forecast-high) (rain forecast-rain)
                   (where forecast-place))
      w
    (let ((what (cond (part (format nil "~a: ~a, ~d degrees, feels like ~d"
                                    (part-name day part) desc temp feels))
                      ((= day 0) (format nil "~a, ~d degrees, feels like ~d" desc temp feels))
                      (t (format nil "~a: ~a, ~d to ~d degrees"
                                 (nth day '("Today" "Tomorrow" "The day after tomorrow"))
                                 desc low high)))))
      (when (>= rain 30)
        (setf what (format nil "~a, ~d percent chance of rain" what rain)))
      (if place
          (format nil "In ~a: ~a" (if (string= where "") place where) what)
          what))))

(defn weather ((day (integer 0)) (place (or null string)) (part (or null (integer 0)))) reply
  (let* ((raw (http-get (format nil "https://wttr.in/~a?format=j1"
                                (substitute #\+ #\Space (or place "")))))
         (w (and raw (parse-weather raw day part))))
    (if (null w)
        (live "Weather" "" (pick *no-weather*))
        (let* ((what (weather-text w day place part))
               (desc (string-downcase (forecast-cond w)))
               (wet (or (>= (forecast-rain w) 60)
                        (some (lambda (x) (search x desc))
                              '("rain" "drizzle" "shower" "snow" "sleet" "thunder"))))
               (feels (if (or (= day 0) part) (forecast-feels w) (forecast-high w))))
          (live "Weather" what
                (choose *weather* what wet *rainy* (< feels 5) *cold* (> feels 26) *hot*))))))

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

(defreplies *battery-low* "~a. Plug me in, I'm fading!" "~a. I'm dying here. Charger, now.")
(defreplies *battery-charging* "~a, and charging. Mmm, juice." "~a and filling up nicely.")
(defreplies *battery-full* "~a. Fully loaded, baby." "~a. I could go all night.")
(defreplies *battery* "Battery is at ~a." "~a. We're good for a while.")

(defn read-file ((path string)) (or null string)
  (handler-case (trim (uiop:read-file-string path))
    (error () nil)))

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

(defreplies *no-mail*
  "No unread mail. Inbox zero, you legend."
  "Nothing new. Nobody loves you. Except me.")

(defcommand info-mail
  "(do|have) i (have|got) [any] [new|unread] (mail|email|emails|mails|messages)"
  "(any|how many) [new|unread] (mail|email|emails|mails)"
  "check [my] (mail|email|emails|inbox)"
  :examples ("Do I have new mail?" "Check my email")
  (let* ((out (output "notmuch" "count" "tag:unread"))
         (n (or (and out (parse-integer out :junk-allowed t)) 0)))
    (live "Mail" (format nil "~d unread" n)
          (cond ((= n 0) (pick *no-mail*))
                ((= n 1) "One unread email. Just one. Go on, read it.")
                ((> n 50) (format nil "~d unread emails. Declaring email bankruptcy?" n))
                (t (format nil "You have ~d unread emails." n))))))

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

(defskill info)
