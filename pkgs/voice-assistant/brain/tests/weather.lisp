;;; Yes or no about the weather, wind, humidity and the sun.

(in-package #:va-tests)

(defun slot-json (time &key (rain "0") (snow "0") (thunder "0") (sun "50") (wind "10")
                         (humidity "60") (feels "12") (uv "1"))
  (list :time time :|chanceofrain| rain :|chanceofsnow| snow :|chanceofthunder| thunder
        :|chanceofsunshine| sun :|windspeedKmph| wind :humidity humidity :|FeelsLikeC| feels
        :|uvIndex| uv :|weatherDesc| (vector (list :value "Cloudy")) :|tempC| feels))

(defun day-json (&rest slots)
  (list :|maxtempC| "14" :|mintempC| "3"
        :astronomy (vector (list :sunrise "07:10 AM" :sunset "04:45 PM"))
        :hourly (coerce slots 'vector)))

(defparameter *wttr*
  (json (list :|current_condition| (vector (list :|weatherDesc| (vector (list :value "Sunny"))
                                                 :|temp_C| "9" :|FeelsLikeC| "6"))
              :weather
              (vector (day-json (slot-json "0") (slot-json "300") (slot-json "600")
                                (slot-json "900") (slot-json "1200" :rain "20")
                                (slot-json "1500" :rain "85" :wind "45")
                                (slot-json "1800" :rain "60") (slot-json "2100" :feels "-2"))
                      (day-json (slot-json "0" :snow "70" :feels "-4")
                                (slot-json "300") (slot-json "600") (slot-json "900" :sun "90")
                                (slot-json "1200" :sun "90" :feels "27" :uv "7")
                                (slot-json "1500" :humidity "90") (slot-json "1800")
                                (slot-json "2100"))))))

(define-test weather-details
  :parent va-tests
  (let ((today (va::sky *wttr* 0 nil 10))
        (later (va::sky *wttr* 0 nil 19))
        (tomorrow (va::sky *wttr* 1 nil 10))
        (night (va::sky *wttr* 1 0 10)))
    (is = 85 (va::sky-rain today))
    (is equal "15:00" (va::sky-rain-at today))
    (is = 60 (va::sky-rain later) "only the hours still ahead")
    (is = 45 (va::sky-wind today))
    (is = -2 (va::sky-coldest today))
    (is = 27 (va::sky-warmest tomorrow))
    (is = 90 (va::sky-humidity tomorrow))
    (is = 0 (va::sky-snow tomorrow) "daytime only")
    (is = 70 (va::sky-snow night))
    (is equal "7:10 am" (va::sky-sunrise today))
    (is equal "4:45 pm" (va::sky-sunset today))
    (flet ((says (what sky)
             (let ((v (va::weather-verdict what sky)))
               (string-downcase (subseq v 0 (1+ (position #\, v)))))))
      (is equal "yes," (says "rain" today))
      (is equal "yes," (says "umbrella" today))
      (is equal "no," (says "snow" today))
      (is equal "yes," (says "windy" today))
      (is equal "yes," (says "jacket" today))
      (is equal "yes," (says "hot" tomorrow))
      (is equal "yes," (says "sunscreen" tomorrow))
      (is equal "no," (says "rain" tomorrow)))
    (false (va::sky "garbage" 0 nil 10))))

(defun ask-of (what day &optional place part)
  (format nil "Info(WeatherAsk { what: ~s, day: ~d, place: ~a, part: ~a })" what day
          (if place (format nil "Some(~s)" place) "None")
          (if part (format nil "Some(~d)" part) "None")))

(define-test weather-ask-phrasings
  :parent va-tests
  (with-focus ()
    (loop for (text . want) in
          '(("Will it rain today?" "rain" 0) ("Is it raining?" "rain" 0)
            ("Do I need an umbrella?" "umbrella" 0) ("Will it rain tomorrow?" "rain" 1)
            ("Will it snow in Berlin tomorrow?" "snow" 1 "berlin")
            ("Do I need a jacket this evening?" "jacket" 0 nil 6)
            ("Will it rain this afternoon?" "rain" 0 nil 5)
            ("Is it cold outside?" "cold" 0) ("How windy is it?" "windy" 0)
            ("Is it going to be hot tomorrow?" "hot" 1) ("How humid is it?" "humid" 0)
            ("When will it rain today?" "rain-when" 0)
            ("Do I need sunscreen tomorrow?" "sunscreen" 1))
          do (is equal (apply #'ask-of want) (parsed text) "~s" text))
    (parses-as "Info(Sun(~s, ~d))"
               '(("When does the sun set?" "sunset" 0) ("When is sunrise tomorrow?" "sunrise" 1)
                 ("What time is sunset?" "sunset" 0)))))
