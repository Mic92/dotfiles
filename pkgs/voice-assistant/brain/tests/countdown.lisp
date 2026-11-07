;;; Counting the days until a date or a named day.

(in-package #:va-tests)

(define-test countdown-days
  :parent va-tests
  (let ((today (va::day-number 2025 10 22)))
    (flet ((until (name) (- (va::event-day name today) today)))
      (is = 64 (until "christmas"))
      (is = 63 (until "christmas eve"))
      (is = 70 (until "new year's eve"))
      (is = 71 (until "new year"))
      (is = 9 (until "halloween"))
      (is = 115 (until "valentine's day"))
      (is = 165 (until "easter"))
      (is = 10 (until "all saints' day"))
      (false (va::event-day "my birthday party" today))))
  (is equal "today" (va::days-away 0))
  (is equal "tomorrow" (va::days-away 1))
  (is equal "in 63 days" (va::days-away 63)))

(define-test countdown-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Info(Countdown(~d, ~s))"
               '(("how many days until christmas" 64 "Christmas")
                 ("how many sleeps till christmas" 64 "Christmas")
                 ("how long until halloween" 9 "Halloween")
                 ("how many days until new year's eve" 70 "New Year's Eve")
                 ("when is easter" 165 "Easter")
                 ("how many days until friday" 2 "Friday, October 24")
                 ("how many days until december 24" 63 "Wednesday, December 24")
                 ("countdown to christmas" 64 "Christmas")))
    (true (search "Christmas" (va::reply-speech
                               (va::perform (va::parse-text "how many days until christmas")))))))
