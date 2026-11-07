;;; Info and holidays, ported from the Rust tests.

(in-package #:va-tests)

(define-test holidays
  :parent va-tests
  (is equal '(2024 3 31) (multiple-value-list (va::civil (va::easter 2024))))
  (is equal '(2025 4 20) (multiple-value-list (va::civil (va::easter 2025))))
  (is equal '(2026 4 5) (multiple-value-list (va::civil (va::easter 2026))))
  (flet ((at (y m d)
           (multiple-value-bind (n name) (va::next-holiday (va::day-number y m d))
             (list (multiple-value-list (va::civil n)) name))))
    (is equal '((2026 11 1) "All Saints' Day") (at 2026 10 25))
    (is equal '((2027 1 1) "New Year's Day") (at 2026 12 26))
    (is equal '((2025 6 9) "Whit Monday") (at 2025 6 1))
    (is equal '((2025 6 19) "Corpus Christi") (at 2025 6 10))))

(defun weather-of (day &optional place part)
  (format nil "Info(Weather { day: ~d, place: ~a, part: ~a })" day
          (if place (format nil "Some(~s)" place) "None")
          (if part (format nil "Some(~d)" part) "None")))

(define-test info-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Info(~a)"
               `(("What time is it?" "Time") ("What time is it right now?" "Time")
                 ("Do you know the time?" "Time") ("How late is it?" "Time")
                 ("What's the current time?" "Time") ("Time, please." "Time")
                 ("What's the date today?" "Date") ("Where am I?" "Where")
                 ("What's my location?" "Where")
                 ("tell me what day today is" "Date") ("current date" "Date")
                 ("can you tell me what today's date is" "Date")
                 ("please tell me what date it is today" "Date") ("what's the current date" "Date")
                 ("tell me the date" "Date") ("do you have the date" "Date")
                 ("what year is it" "Date")
                 ("tell me what time it is right now" "Time")
                 ("can you tell me what time it is please" "Time")
                 ("i need to know the time" "Time")
                 ("what does it say on the clock" "Time")
                 ("what time do you have on your watch" "Time")
                 ("How much battery do I have left?" "Battery")
                 ("What's the battery level?" "Battery")
                 ("How long has the computer been on?" "Uptime") ("Do I have new mail?" "Mail")
                 ("Check my email" "Mail") ("What's in my clipboard?" "Clipboard")
                 ("when is the next holiday, please" "Holiday")
                 ("which holiday is next in line" "Holiday")
                 ("how many days until my next day off" "Holiday")
                 ("is there a holiday next week" "Holiday")
                 ("let me know when my next day off is" "Holiday")
                 ("what's the closest holiday after today" "Holiday")
                 ("when will we have another day off" "Holiday")
                 ("Read the clipboard" "Clipboard")))
    (loop for (text day place part) in
          '(("What's the weather like today?" 0) ("What's the weather tomorrow?" 1)
            ("Weather the day after tomorrow" 2)
            ("What's the weather like tomorrow morning?" 1 nil 3)
            ("How is the weather tonight?" 0 nil 7)
            ("What's the weather in Munich?" 0 "munich")
            ("How is the weather in New York tomorrow?" 1 "new york")
            ("What will the weather be like tomorrow in Berlin?" 1 "berlin"))
          do (is equal (weather-of day place part) (parsed text) "~s" text))))

(defun json (form)
  "FORM as JSON: plists are objects, vectors arrays."
  (labels ((key (k)
             (let ((n (symbol-name k))) (if (string= n (string-upcase n)) (string-downcase n) n)))
           (obj (x)
             (cond ((and (consp x) (keywordp (car x)))
                    (let ((h (make-hash-table :test #'equal)))
                      (loop for (k v) on x by #'cddr
                            do (setf (gethash (key k) h) (obj v)))
                      h))
                   ((vectorp x) (if (stringp x) x (map 'vector #'obj x)))
                   (t x))))
    (com.inuoe.jzon:stringify (obj form))))

(defun hour-json (desc rain)
  (list :|weatherDesc| (vector (list :value desc)) :|temp_C| "12" :|FeelsLikeC| "10"
        :|chanceofrain| rain))

(define-test weather-parsing
  :parent va-tests
  (let* ((hours (coerce (loop for i below 8
                              collect (hour-json (if (= i 4) "Light rain" "Cloudy")
                                                 (if (= i 5) "80" "0")))
                        'vector))
         (json (json (list
                      :|current_condition|
                      (vector (list :|weatherDesc| (vector (list :value "Sunny "))
                                    :|temp_C| "9" :|FeelsLikeC| "6"))
                      :|nearest_area|
                      (vector (list :|areaName| (vector (list :value "Munich"))))
                      :weather (vector (list :|maxtempC| "14" :|mintempC| "3" :hourly hours)
                                       (list :|maxtempC| "20" :|mintempC| "8" :hourly hours)))))
         (today (va::parse-weather json 0 nil))
         (tomorrow (va::parse-weather json 1 nil))
         (evening (va::parse-weather json 1 5)))
    (is equal '("Sunny" 9 6 "Munich")
        (list (va::forecast-cond today) (va::forecast-temp today) (va::forecast-feels today)
              (va::forecast-place today)))
    (is equal '("Light rain" 8 20 80)
        (list (va::forecast-cond tomorrow) (va::forecast-low tomorrow) (va::forecast-high tomorrow)
              (va::forecast-rain tomorrow)))
    (false (va::parse-weather json 2 nil))
    (false (va::parse-weather "Unknown location" 0 nil))
    (is equal '(80 "Cloudy") (list (va::forecast-rain evening) (va::forecast-cond evening))))
  (is equal '(1 3) (multiple-value-list (va::day-part '("tomorrow" "morning"))))
  (is equal '(0 7) (multiple-value-list (va::day-part '("tonight"))))
  (false (va::day-part '("tomorrow")))
  (is equal "Tomorrow morning" (va::part-name 1 3))
  (is equal "new york" (va::place '("new" "york")))
  (false (va::place '("tomorrow"))))

(define-test weather-refines
  :parent va-tests
  (let ((today (va::command-action 'va::info-weather 0 nil nil)))
    (with-focus (:previous today)
      (is equal (weather-of 1) (parsed "and tomorrow?"))
      (is equal (weather-of 0 "berlin") (parsed "what about in berlin"))
      (is equal (weather-of 1 nil 6) (parsed "and tomorrow evening?")))))
