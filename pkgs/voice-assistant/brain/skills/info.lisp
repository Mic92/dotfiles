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
  "!weather [today|now]"
  "!temperature [outside]"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {day}"
  "(what's|what is|how's|how is) the weather [like] (in|at) {place}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] (in|at) {place} {day}"
  "(what's|what is|how's|how is|what will be) the weather [like] [be] {day} (in|at) {place}"
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

(defparameter *conditions*
  '((("rain" "raining" "rainy" "wet" "pour" "pouring") "rain")
    (("snow" "snowing" "snowy") "snow")
    (("cold" "freezing" "chilly") "cold") (("warm") "warm") (("hot") "hot")
    (("sunny" "sun" "nice") "sunny") (("windy" "stormy") "windy") (("humid" "muggy") "humid")
    (("umbrella" "raincoat") "umbrella") (("jacket" "coat" "sweater") "jacket")
    (("sunscreen" "sunblock" "sun cream" "suncream") "sunscreen"))
  "Spoken weather words and the question they ask.")

(defn weather-word ((ws words)) (or null string)
  (let ((spoken (unwords ws)))
    (second (find-if (lambda (c) (member spoken (first c) :test #'string=)) *conditions*))))

(defslot ("condition" "gear") (ws next) (weather-word ws))
(defslot "rain-start" (ws next)
  (and (member (unwords ws) '("rain" "start raining" "be raining" "start to rain") :test #'string=)
       "rain-when"))

(defstruct (sky (:constructor make-sky))
  "The worst and best of the hours a weather question is about."
  (rain 0 :type integer) (rain-at nil :type (or null string)) (snow 0 :type integer)
  (sun 0 :type integer) (wind 0 :type integer) (humidity 0 :type integer)
  (coldest 0 :type integer) (warmest 0 :type integer) (uv 0 :type integer)
  (sunrise "" :type string) (sunset "" :type string))

(defn clock-12 ((s (or null string))) string
  "wttr.in's \"07:10 AM\" as \"7:10 am\"."
  (if (null s) "" (string-downcase (string-left-trim "0" s))))

(defn sky-hours ((hourly vector) (day (integer 0)) (part (or null (integer 0)))
                 (hour (integer 0 23)))
    list
  "The slots asked about: one day part, the rest of today, or tomorrow's daytime."
  (let ((all (coerce hourly 'list)))
    (flet ((starts (h) (floor (or (json-int h "time") 0) 100)))
      (cond (part (let ((h (nth part all))) (and h (list h))))
            ((= day 0) (remove-if (lambda (h) (<= (+ (starts h) 3) hour)) all))
            (t (remove-if (lambda (h) (< (starts h) 6)) all))))))

(defn sky ((raw string) (day (integer 0)) (part (or null (integer 0))) (hour (integer 0 23)))
    (or null sky)
  (let* ((fc (json-get (parse-json raw) "weather" day))
         (hourly (json-get fc "hourly"))
         (hours (and (vectorp hourly) (sky-hours hourly day part hour))))
    (when hours
      (flet ((most (key) (reduce #'max hours :key (lambda (h) (or (json-int h key) 0))))
             (least (key) (reduce #'min hours :key (lambda (h) (or (json-int h key) 0)))))
        (let ((wet (find-if (lambda (h) (>= (or (json-int h "chanceofrain") 0) 50)) hours))
              (astro (json-get fc "astronomy" 0)))
          (make-sky :rain (most "chanceofrain") :snow (most "chanceofsnow")
                    :sun (most "chanceofsunshine") :wind (most "windspeedKmph")
                    :humidity (most "humidity") :coldest (least "FeelsLikeC")
                    :warmest (most "FeelsLikeC") :uv (most "uvIndex")
                    :rain-at (and wet (format nil "~2,'0d:00"
                                              (floor (or (json-int wet "time") 0) 100)))
                    :sunrise (clock-12 (json-get astro "sunrise"))
                    :sunset (clock-12 (json-get astro "sunset"))))))))

(defn chance ((p integer) (yes string) (no string)) string
  (cond ((>= p 50) (format nil "Yes, ~d percent chance. ~a" p yes))
        ((< p 25) (format nil "No, ~d percent chance. ~a" p no))
        (t (format nil "Maybe, ~d percent chance. ~a" p yes))))

(defn weather-verdict ((what string) (s sky)) string
  "The answer to WHAT about S, starting with yes, no or maybe."
  (flet ((yes-no (yes line &rest args)
           (format nil "~:[No~;Yes~], ~?" yes line args)))
    (cond
      ((string= what "rain")
       (chance (sky-rain s) (format nil "Starting around ~a." (or (sky-rain-at s) "noon"))
               "Stay dry, darling."))
      ((string= what "umbrella")
       (chance (sky-rain s) "Take the umbrella, or get wet. Your call." "Leave the umbrella."))
      ((string= what "snow") (chance (sky-snow s) "Snowball fight?" "No snowmen."))
      ((string= what "rain-when")
       (if (sky-rain-at s)
           (format nil "Around ~a, ~d percent chance." (sky-rain-at s) (sky-rain s))
           (format nil "No, no rain coming, ~d percent at most." (sky-rain s))))
      ((string= what "cold") (yes-no (<= (sky-coldest s) 5) "it feels like ~d degrees."
                                     (sky-coldest s)))
      ((string= what "jacket") (yes-no (< (sky-coldest s) 15)
                                       "it feels like ~d degrees at the coldest." (sky-coldest s)))
      ((string= what "warm") (yes-no (>= (sky-warmest s) 18) "up to ~d degrees." (sky-warmest s)))
      ((string= what "hot") (yes-no (>= (sky-warmest s) 25) "up to ~d degrees." (sky-warmest s)))
      ((string= what "sunny") (yes-no (>= (sky-sun s) 60) "~d percent chance of sunshine."
                                      (sky-sun s)))
      ((string= what "windy") (yes-no (>= (sky-wind s) 30) "wind up to ~d kilometers an hour."
                                      (sky-wind s)))
      ((string= what "sunscreen") (yes-no (>= (sky-uv s) 5) "the UV index gets to ~d."
                                          (sky-uv s)))
      ((string= what "humid") (yes-no (>= (sky-humidity s) 80) "humidity up to ~d percent."
                                      (sky-humidity s)))
      (t (format nil "No idea, I'm not a meteorologist.")))))

(defn wttr ((place (or null string))) (or null string)
  (http-get (format nil "https://wttr.in/~a?format=j1" (substitute #\+ #\Space (or place "")))))

(defn refine-weather-ask ((previous action) (ws words)) (or null action)
  "\"and tomorrow?\" after \"will it rain?\""
  (destructuring-bind (what . rest) (action-fields previous)
    (let ((w (refine-weather (apply #'command-action 'info-weather rest) ws)))
      (and w (apply #'command-action 'info-weather-ask what (action-fields w))))))

(defcommand info-weather-ask
  "!(is|will) it [be] [going to] [be] {condition} [today|outside|now|right now]"
  "!(is|will) it [be] [going to] [be] {condition} {day}"
  "!(is|will) it [be] [going to] [be] {condition} {when}"
  "!(is|will) it [be] [going to] [be] {condition} (in|at) {place} [{day}]"
  "!how {condition} (is it|will it be) [today|outside|now]"
  "!how {condition} will it be {day}"
  "!do (i|we) need [a|an|some|my] {gear} [today|outside]"
  "!do (i|we) need [a|an|some|my] {gear} {day}"
  "!do (i|we) need [a|an|some|my] {gear} {when}"
  "!(should i|do i have to) (take|bring|wear) [a|an|some|my] {gear} [today]"
  "!when (will|does|is) it [going to] {rain-start} [today]"
  "!when (will|does|is) it [going to] {rain-start} {day}"
  :kind "WeatherAsk"
  :named t
  :slots ((what coalton:String) (day coalton:UFix) (place (coalton:Optional coalton:String))
          (part (coalton:Optional coalton:UFix)))
  :parse (let ((what (or (slot "condition") (slot "gear") (slot "rain-start")))
               (fields (weather-fields (or (slot "day") (slot "when")) (slot "place"))))
           (and what fields (cons what fields)))
  :refine #'refine-weather-ask
  :filler *weather-filler*
  :examples ("Will it rain tomorrow?" "Do I need an umbrella?")
  (let* ((raw (wttr place))
         (s (and raw (sky raw day part (hour)))))
    (if s
        (live "Weather" what (weather-verdict what s))
        (live "Weather" "" (pick *no-weather*)))))

(defslot "sun-move" (ws next)
  (cond ((member (unwords ws) '("set" "go down" "goes down") :test #'string=) "sunset")
        ((member (unwords ws) '("rise" "come up" "comes up") :test #'string=) "sunrise")))
(defslot "sun-event" (ws next)
  (car (member (unwords ws) '("sunset" "sunrise") :test #'string=)))

(defcommand info-sun
  "!when (does|will|is) the sun [going to] {sun-move} [today|tonight]"
  "!when (does|will|is) the sun [going to] {sun-move} {day}"
  "!(when is|when's|what time is|what's the time of) [the] {sun-event} [today|tonight]"
  "!(when is|when's|what time is|what's the time of) [the] {sun-event} {day}"
  :kind "Sun"
  :slots ((event coalton:String) (day coalton:UFix))
  :parse (let ((e (or (slot "sun-move") (slot "sun-event")))
               (d (day-offset (slot "day"))))
           (and e (<= (or d 0) 2) (list e (or d 0))))
  :examples ("When does the sun set?" "When is sunrise tomorrow?")
  (let* ((raw (wttr nil))
         (s (and raw (sky raw day nil 0)))
         (at (and s (if (string= event "sunset") (sky-sunset s) (sky-sunrise s)))))
    (if (and at (plusp (length at)))
        (live "Sun" at (format nil "~:(~a~) ~a is at ~a." event
                               (nth day '("today" "tomorrow" "the day after tomorrow")) at))
        (live "Sun" "" (pick *no-weather*)))))

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

(defreplies *countdown*
  "~a is ~a, on ~a."
  "~a: ~a, on ~a. Not that I'm counting."
  "~a comes ~a, on ~a. Start planning, slacker.")

(defreplies *countdown-today*
  "~a is today! ~a. Go celebrate, I'll hold the fort."
  "It's ~a today. ~a, in case you lost track. Put the laptop away.")

(defn countdown-fields ((event (or null string)) (day (or null (integer 0) va.types:Date))) list
  (let ((today (context-today *context*)))
    (cond (event (let ((e (event-of (string-downcase event))))
                   (list (- (event-day (first (first e)) today) today) event)))
          (t (let ((n (day-offset day)))
               (and n (list n (spoken-day (+ today n)))))))))

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

(defparameter *inbox* "folder:thalheim.io"
  "The notmuch query for mail sent to me, rather than lists and spam.")

(defcommand info-mail
  "(do|have) i (have|got) [any] [new|unread] (mail|email|emails|mails|messages)"
  "(any|how many) [new|unread] (mail|email|emails|mails)"
  "check [my] (mail|email|emails|inbox)"
  :examples ("Do I have new mail?" "Check my email")
  (let* ((out (output "notmuch" "count" (format nil "~a and tag:unread and date:7d.." *inbox*)))
         (n (or (and out (parse-integer out :junk-allowed t)) 0)))
    (live "Mail" (format nil "~d unread" n)
          (cond ((= n 0) (pick *no-mail*))
                ((= n 1) "One unread email. Just one. Go on, read it.")
                ((> n 50) (format nil "~d unread emails. Declaring email bankruptcy?" n))
                (t (format nil "You have ~d unread emails." n))))))

(defn mail-author ((authors string)) string
  "The first of notmuch's authors, \"a@b.c\" said as \"a at b.c\"."
  (let ((first (trim (subseq authors 0 (position-if (lambda (c) (find c ",|")) authors)))))
    (cl-ppcre:regex-replace-all "@" first " at ")))

(defn mail-subject ((subject string)) string
  "SUBJECT without replies' prefixes, list tags and emoji, at most 12 words."
  (let* ((s (cl-ppcre:regex-replace-all "(?i)^(\\s*(re|aw|antw):\\s*)+" subject ""))
         (s (cl-ppcre:regex-replace-all "\\[[^]]*\\]\\s*" s ""))
         (s (remove-if (lambda (c) (and (> (char-code c) #x2000) (not (alpha-char-p c)))) s))
         (s (trim (cl-ppcre:regex-replace-all "\\s+" s " ")))
         (ws (split-words s)))
    (if (> (length ws) 12) (format nil "~a..." (unwords (subseq ws 0 12))) s)))

(defn read-threads ((raw string)) list
  "(author subject) per thread of `notmuch search --format=json'."
  (let ((threads (parse-json raw)))
    (and (vectorp threads)
         (loop for th across threads
               for authors = (json-get th "authors")
               for subject = (json-get th "subject")
               when (and (stringp authors) (stringp subject))
               collect (list (mail-author authors) (mail-subject subject))))))

(defn inbox-threads ((query string) (most (integer 1))) list
  (read-threads (or (output "notmuch" "search" "--format=json" "--sort=newest-first"
                            (format nil "--limit=~d" most)
                            (format nil "~a and (~a)" *inbox* query))
                    "")))

(defn said-threads ((threads list)) string
  (format nil "~{~a~^ ~}" (loop for (from subject) in threads
                                collect (format nil "From ~a: ~a." from subject))))

(defreplies *mail-read*
  "~a" "Fresh from the inbox. ~a" "~a That's all the gossip.")

(defcommand info-mail-read
  "!(read|tell) [me] [my] [new|latest|last|recent|unread] (mail|mails|email|emails|messages)"
  "!(what are|what's|what is) [in] my (latest|last|new|recent|unread) (mail|mails|email|emails)"
  "!(what's|what is) in my inbox"
  (:join "!who (emailed|mailed|wrote|has written|has emailed|sent) [me] [an email|mail]"
         "[today|lately|recently]")
  :kind "MailRead"
  :examples ("Read my emails" "Who emailed me?")
  (let ((threads (inbox-threads "tag:unread and date:7d.." 3)))
    (if threads
        (live "Mail" (format nil "~d new" (length threads))
              (pick *mail-read* (said-threads threads)))
        (live "Mail" "" (pick *no-mail*)))))

(defreplies *no-mail-from*
  "Nothing from ~a lately. Ghosted?"
  "No mail from ~a. Maybe write first?")

(defcommand info-mail-from
  "!(any|is there|is there any) [new] (mail|email|emails|message) from {person}"
  "!(do|did) i (have|get|got) [any] [new] (mail|email|emails) from {person}"
  "!(did|has) {person} (email|emailed|mail|mailed|write|written|wrote) [to] me"
  "!(when|what) did {person} last (email|write) [me]"
  :kind "MailFrom"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("Any mail from Shannan?" "Did Bob email me?")
  (let ((threads (inbox-threads (format nil "from:\"~a\"" name) 1)))
    (if threads
        (live "Mail" name (said-threads threads))
        (live "Mail" "" (pick *no-mail-from* (string-capitalize name))))))

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
