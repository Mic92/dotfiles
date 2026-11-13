;;; Questions about the weather and the sun.

(in-package #:va)

(in-skill info :continued t)

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

(defreplies *weather-filler*
  "Checking the sky."
  "Let me look outside. Metaphorically."
  "Sticking my head out the window. Brr.")

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
