;;; Duration, clock, date and normalize, ported from the Rust tests.

(in-package #:va-tests)

(defun ws (text) (uiop:split-string text :separator " "))
(defun secs (text) (va::duration (if (string= text "") nil (ws text))))
(defun at (text)
  "Minutes since midnight and whether the time is marked, or nil."
  (let ((c (va::parse-clock (if (string= text "") nil (ws text)))))
    (and c (list (floor (va.types:clock-secs c) 60) (va.types:clock-marked c)))))
(defun hm (h m) (+ (* h 60) m))
(defun date (text) (va::parse-date (ws text)))
(defun same-date (a b) (and a b (equalp (prin1-to-string a) (prin1-to-string b))))

(define-test normalize
  :parent va-tests
  (loop for (text want) in
        '(("Set a 10-minute timer." ("set" "a" "10" "minute" "timer"))
          ("1h30m, 7pm!" ("1" "h" "30" "m" "," "7" "pm"))
          ("Alexa, milk, , eggs," ("alexa" "," "milk" "," "eggs"))
          (", 1,000 euros" ("1000" "euros"))
          ("At 1:30 p.m." ("at" "1:30" "pm"))
          ("Six o'clock" ("six" "o'clock"))
          ("1.5 hours..." ("1.5" "hours"))
          ("Timer for 7.30." ("timer" "for" "7:30"))
          ("at 19.45" ("at" "19:45"))
          ("What is 5+3?" ("what" "is" "5" "plus" "3"))
          ("7×6" ("7" "times" "6")))
        do (is equal want (va::normalize text) "~s" text)))

(define-test durations
  :parent va-tests
  (loop for (text want) in
        '(("5 minutes" 300) ("10 seconds" 10) ("1.5 hours" 5400)
          ("2 hours and 15 minutes" 8100) ("1 h 30 m" 5400)
          ("five minutes" 300) ("twenty five minutes" 1500) ("a minute" 60) ("an hour" 3600)
          ("one hundred and twenty seconds" 120) ("a couple of minutes" 120)
          ("half an hour" 1800) ("an hour and a half" 5400) ("one and a half minutes" 90)
          ("two and a half hours" 9000) ("a minute and a half" 90) ("half a minute" 30)
          ("a quarter of an hour" 900) ("three quarters of an hour" 2700)
          ("an hour and a quarter" 4500)
          ("two minutes thirty" 150) ("1 hour 20" 4800) ("7" 420)
          ("1:30" 90) ("1:30:00" 5400) ("1:75" nil)
          ("" nil) ("0 minutes" nil) ("30 hours" nil))
        do (is eql want (secs text) "~s" text))
  (is eql 300 (va::duration-slot '("5" "minutes") nil))
  (is eql nil (va::duration-slot '("5" "minutes" "please") nil))
  (is eql nil (va::duration-slot '("30") "hours"))
  (is eql 300 (va::duration-slot '("5" "minutes") "and")))

(define-test numbers
  :parent va-tests
  (loop for (text want) in
        '(("42" 42d0) ("twenty five" 25d0) ("one hundred and five" 105d0) ("a hundred" 100d0)
          ("two thousand three hundred" 2300d0) ("minus three" -3d0) ("3.5" 3.5d0)
          ("five apples" nil))
        do (is eql want (va::number-words (ws text)) "~s" text))
  (is eql nil (va::number-words nil)))

(define-test clock-forms
  :parent va-tests
  (loop for (text want) in
        `(("7:30" (,(hm 7 30) nil)) ("7:30 pm" (,(hm 19 30) t)) ("12 am" (0 t))
          ("12 pm" (,(hm 12 0) t)) ("six o'clock" (,(hm 6 0) t))
          ("seven thirty in the evening" (,(hm 19 30) t)) ("seven oh five" (,(hm 7 5) nil))
          ("seven forty five" (,(hm 7 45) nil)) ("half past four" (,(hm 4 30) t))
          ("quarter to five" (,(hm 4 45) t)) ("a quarter past nine" (,(hm 9 15) t))
          ("ten past nine" (,(hm 9 10) t)) ("20 minutes to 6" (,(hm 5 40) t))
          ("19:45" (,(hm 19 45) nil)) ("noon" (,(hm 12 0) t)) ("midnight" (0 t)))
        do (is equal want (at text) "~s" text))
  (dolist (text '("" "25:00" "7:60" "13 pm" "half to five" "five minutes"
                  "seven thirty five forty"))
    (false (at text) "~s" text)))

(define-test clock-occurrences
  :parent va-tests
  (let ((now (* 14 3600)))
    (flet ((from (text) (va::seconds-from (va::parse-clock (ws text)) now))
           (on (text day) (va::seconds-on (va::parse-clock (ws text)) now day)))
      (is = (+ (* 5 3600) 1800) (from "7:30"))
      (is = (+ (* 17 3600) 1800) (from "7:30 am"))
      (is = 3600 (from "3 pm"))
      (is = (* 24 3600) (from "2 pm"))
      (is = (* 22 3600) (from "noon"))
      (is = (* 11 3600) (from "1 o'clock"))
      (is eql (* 17 3600) (on "7" 1))
      (is eql (* 29 3600) (on "7 pm" 1))
      (is eql (* 7 3600) (on "9" 0))
      (is eql nil (on "9 am" 0))
      (is eql nil (on "1 pm" 0))))
  (is equal "7:30 PM" (va::format-hm (+ (* 19 3600) (* 30 60))))
  (is equal "12 PM" (va::format-hm (* 12 3600)))
  (is equal "12:05 AM" (va::format-hm (* 5 60))))

(define-test date-phrasings
  :parent va-tests
  (let ((fri (va.types:Weekday 5 nil))
        (march1 (va.types:MonthDay 3 1)))
    (loop for (text want) in
          `(("friday" ,fri) ("on friday" ,fri) ("this friday" ,fri)
            ("next friday" ,(va.types:Weekday 5 t))
            ("march 1" ,march1) ("march 1st" ,march1) ("march 1 st" ,march1)
            ("the 1 st of march" ,march1) ("march first" ,march1) ("march the first" ,march1)
            ("the first of march" ,march1) ("1st of march" ,march1) ("on march 1st" ,march1)
            ("april 15th" ,(va.types:MonthDay 4 15))
            ("april twenty first" ,(va.types:MonthDay 4 21))
            ("may 2" ,(va.types:MonthDay 5 2)) ("sept 3" ,(va.types:MonthDay 9 3))
            ("the 1st" ,(va.types:OfMonth 1)) ("the fifteenth" ,(va.types:OfMonth 15))
            ("friday the 1 st" ,(va.types:OfMonth 1)) ("thursday the 14 th" ,(va.types:OfMonth 14))
            ("march 4 th 2019" ,(va.types:MonthDay 3 4)) ("tuesday's" ,(va.types:Weekday 2 nil))
            ("monday the 1 st of march" ,march1))
          do (true (same-date want (date text)) "~s: ~s" text (date text))))
  (dolist (text '("the" "of the" "march" "march 32" "may i" "the ball" "next" "1st"))
    (false (date text) "~s" text)))

(define-test dates-from-today
  :parent va-tests
  ;; Wednesday, 2025-10-22.
  (let ((today (va::day-number 2025 10 22)))
    (loop for (text want) in
          '(("wednesday" 0) ("next wednesday" 7) ("friday" 2) ("next friday" 2) ("monday" 5)
            ("october 22" 0) ("october 23" 1) ("march 1" 130) ("the 25th" 3) ("the 1st" 10)
            ("february 30" nil))
          do (is eql want (va::days-ahead (date text) today) "~s" text))))

(defvar *not-a-string* 1
  "Out of the compiler's sight, so only the runtime check can catch it.")

(va::defn wrong-result ((x integer)) string
  (if (> x 0) (princ-to-string x) *not-a-string*))

(define-test defn-checks-results
  :parent va-tests
  (is equal "1" (wrong-result 1))
  (fail (wrong-result -1) 'type-error))
