;;; The timer skill, ported from the Rust tests.

(in-package #:va-tests)

(defmacro at-2pm (&body body)
  `(let ((va::*context* (va::make-context (* 14 3600) (va::day-number 2025 10 22))))
     ,@body))

(defun timer-seconds (text)
  (at-2pm
   (let ((a (va::parse-text text)))
     (and a (string= (va::command-kind (va::action-command a)) "Start")
          (first (va::action-fields a))))))

(defun hms (h m) (+ (* h 3600) (* m 60)))

(define-test timer-durations
  :parent va-tests
  (loop for (text secs) in
        '(("Set a timer for 5 minutes." 300) ("Alexa, set a timer for five minutes please" 300)
          ("Could you please set a timer for 2 minutes for me?" 120) ("Start a 10 minute timer" 600)
          ("Start a 10-minute timer." 600) ("a one-minute timer" 60) ("Ten minute timer" 600)
          ("Timer for an hour and a half." 5400) ("remind me in twenty-five minutes" 1500)
          ("Set the timer to 3 minutes." 180) ("Set timer 7 minutes" 420) ("timer five" 300)
          ("Set a timer for 5 minutes for the pasta." 300) ("Countdown 30 seconds" 30)
          ("Count down from 2 minutes." 120) ("Set an alarm for 10 minutes" 600)
          ("Set an alarm in 10 minutes" 600) ("Wake me up in 20 minutes." 1200)
          ("5 minutes." 300) ("Five minutes, please." 300) ("Timer 1:30" 90)
          ("Set a timer for 1:30:00" 5400) ("Set a timer for 10min" 600)
          ("Set a timer for 1h30" 5400) ("Set a timer for 1h 30m" 5400)
          ("Set a timer for 90 seconds" 90) ("a timer for a quarter of an hour" 900)
          ("a timer for three quarters of an hour" 2700) ("timer for an hour and a quarter" 4500)
          ("timer for a couple of minutes" 120) ("timer for half a minute" 30)
          ("timer for a minute and a half" 90) ("timer for 2 and a half minutes" 150)
          ("Set a timer for 5 minutes and 30 seconds." 330)
          ("Set a timer for one hundred and twenty seconds." 120)
          ("SET A TIMER FOR FIVE MINUTES" 300))
        do (is eql secs (timer-seconds text) "~s" text)))

(define-test timer-times-of-day
  :parent va-tests
  (loop for (text secs) in
        `(("Set an alarm for 7:30" ,(hms 5 30)) ("Set an alarm for 7:30 am" ,(hms 17 30))
          ("Set an alarm for 7:30 a.m." ,(hms 17 30)) ("Timer until 3 pm" ,(hms 1 0))
          ("Set a timer for 3pm" ,(hms 1 0)) ("Set a timer for 3 o'clock" ,(hms 1 0))
          ("Wake me up at 6 o'clock." ,(hms 4 0)) ("Alarm at half past four" ,(hms 2 30))
          ("Alarm at quarter to five" ,(hms 2 45)) ("Remind me at noon" ,(hms 22 0))
          ("Set an alarm for midnight" ,(hms 10 0)) ("Alarm at 19:45" ,(hms 5 45))
          ("Set an alarm at seven thirty in the evening" ,(hms 5 30))
          ("Set an alarm for 1:05 pm" ,(hms 23 5)) ("Alarm at seven oh five" ,(hms 5 5))
          ("Alarm at ten past nine" ,(hms 7 10)) ("Set an alarm for 10" ,(hms 8 0))
          ("Set an alarm for tomorrow at 7" ,(hms 17 0))
          ("Set a timer for tomorrow at 9 am" ,(hms 19 0))
          ("Wake me up at 6:30 tomorrow." ,(hms 16 30))
          ("Set an alarm for 7 pm tomorrow" ,(hms 29 0))
          ("Timer until 3 pm today" ,(hms 1 0)) ("Alarm today at 9" ,(hms 7 0)))
        do (is eql secs (timer-seconds text) "~s" text)))

(define-test timer-controls
  :parent va-tests
  (loop for (text want) in
        '(("Cancel the timer." "Cancel") ("Stop timer" "Cancel") ("Turn off the timer." "Cancel")
          ("Stop the alarm!" "Cancel") ("Dismiss the alarm" "Cancel") ("Kill the timer" "Cancel")
          ("Pause the timer" "Pause") ("How much time is left?" "Left")
          ("How much time is left on the timer?" "Left")
          ("How long until the alarm goes off?" "Left") ("Is a timer running?" "Left")
          ("How much longer?" "Left") ("Resume the timer." "Pause") ("Continue the timer" "Pause"))
        do (is equal (format nil "Timer(~a)" want) (at-2pm (parsed text)) "~s" text)))

(define-test timer-rejects
  :parent va-tests
  (dolist (text '("The timer is ready." "set a timer" "" "..." "I'll be there in 5 minutes."
                  "Add 5 minutes to the timer." "Set a timer for 0 minutes"
                  "Set a timer for 30 hours" "Alarm today at 9 am"))
    (is equal "-" (at-2pm (parsed text)) "~s" text)))

(define-test timer-asks-for-missing-time
  :parent va-tests
  (dolist (text '("Set a timer for tomorrow." "Alarm tomorrow" "Wake me up tomorrow"))
    (is equal "Timer(When { day: 1 })" (at-2pm (parsed text)) "~s" text))
  (at-2pm
   (let ((ask (va::reply-question
               (va::perform (va::parse-text "alarm tomorrow")))))
     (loop for (answer secs) in `(("7 am" ,(hms 17 0)) ("At 7." ,(hms 17 0))
                                  ("half past eight" ,(hms 18 30)))
           do (is eql secs (first (va::action-fields (funcall (va::question-answer ask) answer)))
                  "~s" answer)))))

(define-test timer-replies
  :parent va-tests
  (is equal "1 minute and 30 seconds" (va::spoken-duration 90))
  (is equal "2 hours" (va::spoken-duration 7200))
  (flet ((body (text)
           (at-2pm (va::reply-body (va:fake-effects-value (va::perform (va::parse-text text)))))))
    (is equal "7:30 PM" (body "Set an alarm for 7:30"))
    (is equal "7 AM" (body "Set an alarm for tomorrow at 7"))
    (is equal "1 minute and 30 seconds" (body "Timer for 90 seconds"))))

(defun in-pool (table what said)
  (some (lambda (l) (string= (format nil l what) said)) (va::replies-lines table)))

(define-test timer-conditional-replies
  :parent va-tests
  (at-2pm
   (loop repeat 5
         do (va:fake-effects
             (true (in-pool va::*short* "30 seconds"
                            (va::reply-speech (va::start 30 nil (* 14 3600)))))
             (true (in-pool va::*pasta* "10 minutes"
                            (va::reply-speech (va::start 600 nil (* 14 3600)))))
             (true (in-pool va::*early-alarm* "6:30 AM"
                            (va::reply-speech (va::start 59400 t (* 14 3600)))))))))

(define-test timer-left-replies
  :parent va-tests
  (let ((va::*context* (va::make-context (* 10 3600) 0)))
    (true (in-pool va::*no-timer* nil (va::reply-speech (va::time-left nil nil nil))))
    (is equal "1 minute and 30 seconds" (va::reply-body (va::time-left 90 nil nil)))
    (is equal "30 minutes, the alarm rings at 10:30 AM"
        (va::reply-body (va::time-left 1800 t nil)))
    (true (search "paused" (string-downcase (va::reply-speech (va::time-left 120 nil t)))))))
