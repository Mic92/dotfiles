;;; Ported from calc.rs.

(in-package #:va-tests)

(defun calc (text)
  "The Calc action TEXT parses to, as Rust's Debug prints it."
  (with-focus () (parsed text)))

(define-test calc-conversions
  :parent va-tests
  (with-focus ()
    (loop for (text value from to) in
          '(("How many cups are in a liter?" 1 "liters" "cups")
            ("convert 5 miles to kilometers" 5 "miles" "kilometers")
            ("What is 100 degrees fahrenheit in celsius" 100 "degrees Fahrenheit"
             "degrees Celsius")
            ("how many ounces in a pound" 1 "pounds" "ounces")
            ("what's 3 feet in centimeters" 3 "feet" "centimeters")
            ("how many teaspoons are there in a tablespoon" 1 "tablespoons" "teaspoons")
            ("how many ounces are in a cup" 1 "cups" "fluid ounces")
            ("how do I convert pounds to kilograms" 1 "pounds" "kilograms")
            ("12 feet is equal to how many inches" 12 "feet" "inches")
            ("what's the conversion between tablespoons and cups" 1 "tablespoons" "cups")
            ("twenty kilometers per hour in miles per hour" 20 "kilometers per hour"
             "miles per hour"))
          for unit = (lambda (plural) (position plural va::*units* :key #'va::unit-plural
                                                :test #'string=))
          do (is equal (format nil "Calc(Convert { value: ~,1f, from: ~d, to: ~d })"
                               value (funcall unit from) (funcall unit to))
                 (parsed text) "~s" text))
    (parses-not "Calc" '("how many miles in a kilogram"))))

(define-test calc-maths
  :parent va-tests
  (with-focus ()
    (parses-as "Calc(Math(~s))"
               '(("What is 5 plus 3?" "5 + 3") ("What's 7×6?" "7 * 6")
                 ("What is twenty five times four?" "25 * 4")
                 ("Calculate 100 divided by 8" "100 / 8")
                 ("What's the square root of 81?" "sqrt(81)")
                 ("What is 20 percent of 50?" "20 / 100 * 50") ("What's 12 squared?" "12^2")
                 ("How much is 10 minus 15?" "10 - 15") ("What is 2 to the power of 10" "2^10")
                 ("what's 3 plus 4 times 2" "3 + 4 * 2")
                 ("please add 456 and 781 for me" "456 + 781")
                 ("add twelve and twelve please" "12 + 12")
                 ("what is the sum of 3 plus 5" "3 + 5") ("subtract 100 from 12" "12 - 100")
                 ("multiply 7 and 42" "7 * 42") ("what is 8 factorial" "8!")
                 ("can you tell me what 30% off 279 is" "279 * (1 - 30 / 100)")
                 ("what is an 18 percent tip on 20 dollars" "20 * 18 / 100")
                 ("i need to know what is 85% of $1500" "85 / 100 * 1500")
                 ("what is 3/4 of 18" "3 / 4 * 18") ("what is 10 to the 12th power" "10^12")
                 ("how many times does 8 go into 2000" "2000 / 8")
                 ("what do you get if you divide 3 by 2" "3 / 2")
                 ("what does 6 x 1 equal" "6 * 1") ("560 divided by 3" "560 / 3")
                 ("can you tell me the square root of pi" "sqrt(pi)")))
    (parses-not "Calc" '("what is the meaning of life")))
  (is equal "12.5" (va::spoken-number 12.5d0))
  (is equal "42" (va::spoken-number 42d0))
  (is equal "0.33" (va::spoken-number (/ 1d0 3))))

(define-test calc-time-zones
  :parent va-tests
  (with-focus ()
    (parses-as "Calc(Time { at: ~a, from: ~a, to: ~a })"
               '(("What time is it in Tokyo?" "None" "None" "Some(\"Asia/Tokyo\")")
                 ("what's the time in new york right now" "None" "None"
                  "Some(\"America/New_York\")")
                 ("what time is it in california" "None" "None" "Some(\"America/Los_Angeles\")")
                 ("what is 3 pm here in new york" "Some(54000)" "None"
                  "Some(\"America/New_York\")")
                 ("what's 3 pm in new york in tokyo time" "Some(54000)"
                  "Some(\"America/New_York\")" "Some(\"Asia/Tokyo\")")
                 ("convert 3 pm london time to sydney time" "Some(54000)"
                  "Some(\"Europe/London\")" "Some(\"Australia/Sydney\")")
                 ("when it's 3 pm in new york what time is it here" "Some(54000)"
                  "Some(\"America/New_York\")" "None")
                 ("what time will it be in india at 3 pm" "Some(54000)" "None"
                  "Some(\"Asia/Kolkata\")")))
    (parses-as "Calc(DateIn(~d))"
               '(("what day will it be in fourteen days" 14) ("in 2 days, what date will it be" 2)
                 ("what will the date be 100 days from now" 100)
                 ("what's the date 400 days from now" 400)
                 ("i need to know the date in 256 days" 256)))
    (parses-as "Calc(Zone(~s))"
               '(("what timezone is london in" "Europe/London")
                 ("what's london's timezone" "Europe/London")
                 ("what is the timezone for la" "America/Los_Angeles")
                 ("in which time zone does denver reside" "America/Denver")
                 ("tell me the timezone for hong kong" "Asia/Hong_Kong")
                 ("what timezone is hanoi in" "Asia/Ho_Chi_Minh")
                 ("timezone in salt lake city" "America/Denver")
                 ("do you know what timezone italy is in" "Europe/Rome")
                 ("italy is in what timezone" "Europe/Rome")
                 ("tell me the timezone that spain is in" "Europe/Madrid")
                 ("what timezone does lisbon have" "Europe/Lisbon")
                 ("timezone of cuba" "America/Havana")
                 ("in paris, what's the timezone" "Europe/Paris")
                 ("tell me what time zone portugal uses" "Europe/Lisbon")
                 ("what timezone is detroit in" "America/Detroit")
                 ("what timezone is vegas in" "America/Los_Angeles")
                 ("french time zone" "Europe/Paris")
                 ;; From tzdata, which Rust's table did not have.
                 ("what timezone is reykjavik in" "Atlantic/Reykjavik")
                 ("what timezone is iceland in" "Atlantic/Reykjavik")))))

(define-test calc-numbat-text
  :parent va-tests
  (is equal '("GMT" "UTC" 72600) (multiple-value-list (va::zone-info "GMT +00:00 20:10")))
  (is equal '("IST" "UTC plus 5:30" 6000) (multiple-value-list (va::zone-info "IST +05:30 01:40")))
  (is equal '("" "UTC minus 3" 61800) (multiple-value-list (va::zone-info "-03 -03:00 17:10")))
  (true (search "now() -> tz(\"Asia/Tokyo\")" (va::time-expr nil nil "Asia/Tokyo")))
  (let ((e (va::time-expr (+ (* 15 3600) (* 30 60)) "America/New_York" nil)))
    (true (search "15:30 America/New_York\")" e) "~a" e)
    (true (search "-> local" e) "~a" e))
  (is equal '(75900 1) (multiple-value-list (va::shifted "298 299 21:05")))
  (is equal '(82800 -1) (multiple-value-list (va::shifted "1 365 23:00")))
  (is = 8.04672d0 (va::parse-real "8.04672"))
  (false (va::convertible (va::find-unit '("mile")) (va::find-unit '("gram")))))
