;;; Maths and unit conversion, worked out by numbat.

(in-package #:va)

(in-skill calc)

(defparameter *fixed-offsets*
  '(("CET" . "Etc/GMT-1") ("CEST" . "Etc/GMT-2") ("WET" . "Etc/GMT") ("WEST" . "Etc/GMT-1")
    ("EET" . "Etc/GMT-2") ("EEST" . "Etc/GMT-3") ("BST" . "Etc/GMT-1") ("MSK" . "Etc/GMT-3")
    ("EST" . "Etc/GMT+5") ("EDT" . "Etc/GMT+4") ("CST" . "Etc/GMT+6") ("CDT" . "Etc/GMT+5")
    ("MST" . "Etc/GMT+7") ("MDT" . "Etc/GMT+6") ("PST" . "Etc/GMT+8") ("PDT" . "Etc/GMT+7")
    ("AKST" . "Etc/GMT+9") ("HST" . "Etc/GMT+10") ("JST" . "Etc/GMT-9") ("KST" . "Etc/GMT-9")
    ("SGT" . "Etc/GMT-8") ("HKT" . "Etc/GMT-8") ("AEST" . "Etc/GMT-10") ("AEDT" . "Etc/GMT-11")
    ("NZST" . "Etc/GMT-12") ("NZDT" . "Etc/GMT-13") ("IST" . "Asia/Kolkata"))
  "Abbreviations are fixed offsets: \"3 pm EST\" is UTC-5 even in summer.
Etc/GMT signs are inverted.")

(defparameter *zone-aliases*
  '(("America/New_York" "eastern" "nyc" "new york city" "east coast" "eastern time" "boston"
     "washington" "miami" "atlanta" "philadelphia" "tampa" "bangor" "maine" "hartford" "orlando"
     "florida")
    ("America/Los_Angeles" "pacific" "southern california" "la" "san francisco" "california"
     "seattle" "west coast" "pacific time" "silicon valley" "las vegas" "vegas" "reno" "nevada"
     "san diego" "sacramento" "portland" "oregon")
    ("America/Chicago" "central" "tx" "texas" "houston" "dallas" "austin" "central time" "missouri"
     "st louis" "kansas city")
    ("America/Denver" "mountain" "mountain time" "colorado" "utah" "salt lake city")
    ("America/Detroit" "michigan")
    ("America/Boise" "idaho")
    ("America/Anchorage" "alaska")
    ("America/Lima" "peruvian")
    ("America/Guayaquil" "ecuador" "quito")
    ("America/Jamaica" "kingston")
    ("Asia/Pyongyang" "north korea")
    ("America/Phoenix" "arizona")
    ("America/Vancouver" "british columbia")
    ("Asia/Ho_Chi_Minh" "hanoi" "saigon" "ho chi minh city")
    ("Europe/Madrid" "valencia" "spanish" "spain" "barcelona")
    ("Pacific/Honolulu" "hawaii" "hawaiian")
    ("America/Toronto" "montreal" "canada")
    ("America/Mexico_City" "mexico")
    ("America/Sao_Paulo" "brazil" "brasilia" "rio" "rio de janeiro")
    ("America/Argentina/Buenos_Aires" "argentina")
    ("Europe/London" "british" "greenwich" "greenwhich" "uk" "england" "the uk" "scotland"
     "edinburgh")
    ("Europe/Lisbon" "portugal")
    ("Europe/Paris" "french")
    ("Europe/Berlin" "german" "munich" "hamburg" "frankfurt")
    ("Europe/Amsterdam" "holland")
    ("Europe/Rome" "italian" "milan")
    ("Europe/Kyiv" "kiev")
    ("Europe/Moscow" "russia")
    ("Asia/Jerusalem" "tel aviv")
    ("Africa/Johannesburg" "cape town")
    ("Asia/Dubai" "uae" "abu dhabi")
    ("Asia/Kolkata" "indian" "delhi" "new delhi" "mumbai" "bangalore")
    ("Asia/Shanghai" "china" "chinese" "beijing")
    ("Asia/Tokyo" "japanese" "osaka")
    ("Asia/Seoul" "south korea")
    ("Australia/Sydney" "australia" "melbourne")
    ("UTC" "utc" "gmt")
    ("CET" "cet" "c e t" "central european time")
    ("CEST" "cest" "c e s t" "central european summer time")
    ("WET" "wet" "w e t" "western european time")
    ("WEST" "west" "w e s t" "western european summer time")
    ("EET" "eet" "e e t" "eastern european time")
    ("EEST" "eest" "e e s t" "eastern european summer time")
    ("BST" "bst" "b s t" "british summer time")
    ("MSK" "msk" "m s k" "moscow time")
    ("EST" "est" "e s t" "eastern standard time")
    ("EDT" "edt" "e d t" "eastern daylight time")
    ("CST" "cst" "c s t" "central standard time")
    ("CDT" "cdt" "c d t" "central daylight time")
    ("MST" "mst" "m s t" "mountain standard time")
    ("MDT" "mdt" "m d t" "mountain daylight time")
    ("PST" "pst" "p s t" "pacific standard time")
    ("PDT" "pdt" "p d t" "pacific daylight time")
    ("AKST" "akst" "a k s t" "alaska time")
    ("HST" "hst" "h s t" "hawaii standard time")
    ("JST" "jst" "j s t" "japan standard time")
    ("KST" "kst" "k s t" "korea standard time")
    ("SGT" "sgt" "s g t" "singapore time")
    ("HKT" "hkt" "h k t" "hong kong time")
    ("AEST" "aest" "a e s t" "australian eastern standard time")
    ("AEDT" "aedt" "a e d t" "australian eastern daylight time")
    ("NZST" "nzst" "n z s t" "new zealand standard time")
    ("NZDT" "nzdt" "n z d t" "new zealand daylight time")
    ("IST" "ist" "i s t" "indian standard time"))
  "Names tzdata does not know, for an IANA zone or one of *fixed-offsets*:
nicknames, states, demonyms and countries with several zones.")

(defvar *places* nil
  "Spoken place to zone, from tzdata and *zone-aliases*. Read on first use.")

(defn tzdata ((file string)) list
  "The rows of a tzdata table, split at tabs."
  (with-open-file (in (merge-pathnames file (uiop:ensure-directory-pathname
                                             (or (uiop:getenv "TZDIR") "/etc/zoneinfo"))))
    (loop for line = (read-line in nil) while line
          unless (uiop:string-prefix-p "#" line)
          collect (uiop:split-string line :separator '(#\Tab)))))

(defn zone-city ((iana string)) string
  "\"America/New_York\" -> \"New York\"."
  (substitute #\Space #\_ (subseq iana (1+ (or (position #\/ iana :from-end t) -1)))))

(defn country-zone ((rows list)) (or null string)
  "A country's zone: its only one, or the one tzdata says covers most of it."
  (let ((most (remove-if-not (lambda (r) (uiop:string-prefix-p "most" (string-downcase
                                                                       (or (fourth r) ""))))
                             rows)))
    (third (cond ((null (rest rows)) (first rows)) ((null (rest most)) (first most))))))

(defn read-places () hash-table
  (let ((places (make-hash-table :test #'equal)) (zones (tzdata "zone.tab")))
    (dolist (row (tzdata "iso3166.tab"))
      (destructuring-bind (code name) row
        (let ((zone (country-zone (remove code zones :key #'first :test-not #'string=))))
          (when zone
            (setf (gethash (string-downcase (cl-ppcre:regex-replace " \\(.*\\)" name "")) places)
                  zone)))))
    (dolist (row zones)
      (setf (gethash (string-downcase (zone-city (third row))) places) (third row)))
    (loop for (zone . names) in *zone-aliases*
          do (dolist (n names) (setf (gethash n places) zone)))
    places))

(defstruct (unit (:constructor make-unit (singular plural dimension numbat &rest names)))
  "A unit, how numbat writes it and the other names it is heard as."
  (singular "" :type string) (plural "" :type string) (dimension nil :type keyword)
  (numbat "" :type string) (names nil :type list))

(defparameter *units*
  (map 'vector (lambda (u) (apply #'make-unit u))
       '(("millimeter" "millimeters" :length "mm" "mm" "millimetre" "millimetres")
         ("centimeter" "centimeters" :length "cm" "cm" "centimetre" "centimetres")
         ("meter" "meters" :length "m" "m" "metre" "metres")
         ("kilometer" "kilometers" :length "km" "km" "kilometre" "kilometres" "k")
         ("inch" "inches" :length "inch" "in")
         ("foot" "feet" :length "ft" "ft")
         ("yard" "yards" :length "yard" "yd")
         ("mile" "miles" :length "mile" "mi")
         ("milligram" "milligrams" :mass "mg" "mg" "milligramme" "milligrammes")
         ("gram" "grams" :mass "g" "g" "gramme" "grammes")
         ("kilogram" "kilograms" :mass "kg" "kg" "kilo" "kilos" "kilogramme")
         ("ounce" "ounces" :mass "oz" "oz")
         ("pound" "pounds" :mass "lb" "lb" "lbs")
         ("stone" "stone" :mass "stone" "stones")
         ("ton" "tons" :mass "tonne" "tonne" "tonnes" "metric ton" "metric tons")
         ("milliliter" "milliliters" :volume "mL" "ml" "millilitre" "millilitres")
         ("liter" "liters" :volume "L" "l" "litre" "litres")
         ("teaspoon" "teaspoons" :volume "tsp" "tsp")
         ("tablespoon" "tablespoons" :volume "tbsp" "tbsp")
         ("cup" "cups" :volume "cup")
         ("fluid ounce" "fluid ounces" :volume "fluidounce" "fl oz")
         ("pint" "pints" :volume "pint")
         ("quart" "quarts" :volume "quart")
         ("gallon" "gallons" :volume "gallon")
         ("degree Celsius" "degrees Celsius" :temperature "°C" "celsius" "centigrade"
          "degrees celsius"
          "degree celsius" "degrees centigrade" "c")
         ("degree Fahrenheit" "degrees Fahrenheit" :temperature "°F" "fahrenheit"
          "degrees fahrenheit"
          "degree fahrenheit" "f")
         ("kelvin" "kelvin" :temperature "K" "kelvins")
         ("kilometer per hour" "kilometers per hour" :speed "km/h" "km/h" "kmh"
          "kilometres per hour"
          "kph")
         ("mile per hour" "miles per hour" :speed "mph" "mph")
         ("meter per second" "meters per second" :speed "m/s" "metres per second")
         ("knot" "knots" :speed "knot")
         ("euro" "euros" :money "EUR" "eur" "€")
         ("US dollar" "US dollars" :money "USD" "dollar" "dollars" "usd" "us dollar" "us dollars"
          "american dollar" "american dollars" "buck" "bucks")
         ("British pound" "British pounds" :money "GBP" "british pound" "british pounds"
          "pound sterling" "pounds sterling" "sterling" "quid" "gbp")
         ("Swiss franc" "Swiss francs" :money "CHF" "franc" "francs" "swiss franc" "swiss francs"
          "chf")
         ("Japanese yen" "Japanese yen" :money "JPY" "yen" "japanese yen")
         ("Chinese yuan" "Chinese yuan" :money "CNY" "yuan" "renminbi" "chinese yuan")
         ("Canadian dollar" "Canadian dollars" :money "CAD" "canadian dollar" "canadian dollars")
         ("Australian dollar" "Australian dollars" :money "AUD" "australian dollar"
          "australian dollars" "aussie dollars")
         ("Thai baht" "Thai baht" :money "THB" "baht" "thai baht")
         ("Swedish krona" "Swedish kronor" :money "SEK" "swedish krona" "swedish kronor"
          "swedish crowns")
         ("Danish krone" "Danish kroner" :money "DKK" "danish krone" "danish kroner"
          "danish crowns")
         ("Norwegian krone" "Norwegian kroner" :money "NOK" "norwegian krone" "norwegian kroner"
          "norwegian crowns")
         ("Polish zloty" "Polish zlotys" :money "PLN" "zloty" "zlotys" "polish zloty")
         ("Czech koruna" "Czech korunas" :money "CZK" "czech koruna" "czech crowns")
         ("Indian rupee" "Indian rupees" :money "INR" "rupee" "rupees" "indian rupees")
         ("Hungarian forint" "Hungarian forints" :money "HUF" "forint" "forints")
         ("Turkish lira" "Turkish lira" :money "TRY" "lira" "turkish lira")))
  "Commands carry indices into this, like Rust's UNITS.")

(defn rust-float ((v real)) string
  "V as Rust's Display prints a float: 5 and 2.5, not 5.0 and 2.5d0."
  (if (= v (ftruncate v))
      (format nil "~d" (truncate v))
      (let ((*read-default-float-format* 'double-float)) (princ-to-string (float v 1d0)))))

(defn parse-real ((s string)) (or null double-float)
  "A plain decimal number, as numbat prints one."
  (and (cl-ppcre:scan "^-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?$" s)
       (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
         (float (read-from-string s) 1d0))))

(defn find-zone ((ws words)) (or null string)
  "\"sydney, australia\" and \"houston, tx\": the city decides."
  (let* ((ws (strip-one (subseq ws 0 (position "," ws :test #'string=)) "the"))
         (tail (find-if (lambda (tail) (and (< (length tail) (length ws))
                                            (a:ends-with-subseq tail ws :test #'string=)))
                        '(("time" "zone") ("timezone") ("time"))))
         (name (unwords (butlast ws (length tail))))
         (name (if (uiop:string-suffix-p name "'s") (subseq name 0 (- (length name) 2)) name)))
    (values (gethash name (or *places* (setf *places* (read-places)))))))

(defn iana ((zone string)) string
  (or (cdr (assoc zone *fixed-offsets* :test #'string=)) zone))

(defn find-unit ((ws words)) (or null (integer 0))
  "\"kilometers per hour\", \"a liter\", \"degrees celsius\"."
  (let ((name (unwords (strip-one ws "a" "an" "one" "the"))))
    (position-if (lambda (u) (or (string-equal name (unit-singular u))
                                 (string-equal name (unit-plural u))
                                 (member name (unit-names u) :test #'string=)))
                 *units*)))

(defn unit-at ((i (integer 0))) unit (aref *units* i))

(defn convertible ((from (integer 0)) (to (integer 0))) boolean
  (eq (unit-dimension (unit-at from)) (unit-dimension (unit-at to))))

(defn fluid ((a (integer 0)) (b (integer 0))) (integer 0)
  "\"Ounces in a cup\" means fluid ounces."
  (if (and (string= (unit-singular (unit-at a)) "ounce")
           (eq (unit-dimension (unit-at b)) :volume))
      (position "fluid ounce" *units* :key #'unit-singular :test #'string=)
      a))

(defn sterling ((a (integer 0)) (b (integer 0))) (integer 0)
  "\"Pounds in euros\" means British pounds."
  (if (and (string= (unit-singular (unit-at a)) "pound")
           (eq (unit-dimension (unit-at b)) :money))
      (position "British pound" *units* :key #'unit-singular :test #'string=)
      a))

(defn money ((x double-float)) string
  "Cents for amounts, three significant digits below one."
  (cond ((= x (fround x)) (format nil "~d" (round x)))
        ((>= (abs x) 1) (format nil "~,2f" x))
        (t (spoken-3 x))))

(defparameter *operators*
  '((("plus") " + ") (("+") " + ") (("minus") " - ") (("-") " - ") (("times") " * ")
    (("x") " * ") (("×") " * ") (("*") " * ") (("multiplied" "by") " * ")
    (("divided" "by") " / ") (("over") " / ") (("/") " / ") (("to" "the" "power" "of") "^")
    (("percent" "of") " / 100 * ") (("of") " * "))
  "Spoken operators and how numbat writes them, first match wins.")

(defparameter *noise* '("dollars" "dollar" "euros" "euro" "bucks")
  "Words after a number that change nothing, as in \"20 dollars\".")

(defn leading-number ((ws words)) (values (or null double-float) (integer 0))
  "The longest run of number words at the start, as in \"twenty five times\"."
  (loop for n from (length ws) downto 1
        for v = (number-words (subseq ws 0 n))
        when v return (values v n)
        finally (return (values nil 0))))

(defn power-of ((w string)) (or null (integer 0))
  "\"12th\", \"3\", \"sixth\" -> 12, 3, 6."
  (or (ordinal w) (digits (string-right-trim "stndrh" w))))

(defn nth-power ((w string) (rest words)) (values (or null string) words)
  (let ((n (power-of w))) (if n (values (format nil "^~d" n) rest) (values nil rest))))

(defn expression-term ((ws words)) (or null string)
  "One number with noise after it."
  (let* ((ws (strip-one ws "a" "an"))
         (end (position-if-not (lambda (w) (member w *noise* :test #'string=)) ws
                               :from-end t)))
    (when end
      (multiple-value-bind (v n) (leading-number (subseq ws 0 (1+ end)))
        (and v (= n (1+ end)) (rust-float v))))))

(defn percent-expression ((ws words)) (or null string)
  "\"30 percent off 279\", \"an 18 percent tip on 20 dollars\"."
  (let ((p (position "percent" ws :test #'string=)))
    (multiple-value-bind (kind rest)
        (trivia:match (and p (nthcdr (1+ p) ws))
          ((list* "off" "of" r) (values :off r))
          ((list* "off" r) (values :off r))
          ((list* "tip" (or "on" "for" "of") r) (values :tip r)))
      (let ((a (and kind (expression-term (subseq ws 0 p))))
            (b (and kind (expression-term rest))))
        (when (and a b)
          (format nil (if (eq kind :off) "~a * (1 - ~a / 100)" "~a * ~a / 100") b a))))))

(defn power-suffix ((ws words)) (values (or null string) words)
  "\"squared\", \"to the 3rd power\" and the like at the start of WS."
  (trivia:match ws
    ((list* "squared" r) (values "^2" r))
    ((list* "cubed" r) (values "^3" r))
    ((list* "factorial" r) (values "!" r))
    ((list* "to" "the" n (or "th" "st" "nd" "rd") "power" r) (nth-power n r))
    ((list* "to" "the" n "power" r) (nth-power n r))
    (_ (values nil ws))))

(defn starts-with-words ((prefix list) (ws words)) boolean
  (and (a:starts-with-subseq prefix ws :test #'string=) t))

(defn term ((ws words)) (values (or null string) words (integer 0))
  "A number or pi, maybe under a square root, with powers after it. Also the
words left and how many operations it has."
  (let* ((root (starts-with-words '("square" "root" "of") ws))
         (ws (if root (nthcdr 3 ws) ws))
         (ops (if root 1 0)))
    (multiple-value-bind (v n) (if (equal (first ws) "pi") (values "pi" 1) (leading-number ws))
      (if (null v)
          (values nil ws 0)
          (let ((ws (member-if-not (lambda (w) (member w *noise* :test #'string=)) (nthcdr n ws)))
                (text (if (stringp v) v (rust-float v))))
            (values (with-output-to-string (out)
                      (format out (if root "sqrt(~a)" "~a") text)
                      (loop (multiple-value-bind (suffix r) (power-suffix ws)
                              (unless suffix (return))
                              (write-string suffix out)
                              (setf ws r)
                              (incf ops))))
                    ws ops))))))

(defn expression ((ws words)) (or null string)
  "\"5 plus 3\", \"square root of 81\", \"2 to the power of 10\" as numbat
expressions. Only the shape is checked, numbat computes it once when answering."
  (or (percent-expression ws)
      (loop with ops = 0
            for (text rest n) = (multiple-value-list (term ws))
            for op = (and text rest (find rest *operators* :key #'first
                                          :test (lambda (ws p) (starts-with-words p ws))))
            unless text return nil
            collect text into pieces
            do (incf ops n)
            when (equal rest '("percent")) return (format nil "~{~a~} / 100" pieces)
            when (null rest) return (and (plusp ops) (format nil "~{~a~}" pieces))
            unless op return nil
            collect (second op) into pieces
            do (setf ws (nthcdr (length (first op)) rest))
            (incf ops))))

(defslot "expr" (ws next) (expression ws))
(defslot ("from" "to") (ws next) (find-unit ws))
(defslot ("zone" "origin") (ws next) (find-zone ws))

(defn numbat ((expr string)) (or null string)
  (let ((out (output "numbat" "-e" expr))) (and out (string-trim "\"" (trim out)))))

(defn numbat-number ((expr string)) (or null double-float)
  "numbat's leading number, as in \"8.04672 km\"."
  (let ((out (numbat expr)))
    (and out (parse-real (remove #\, (first (split-words out)))))))

(defn spoken-number ((v double-float)) string
  (if (sb-ext:float-infinity-p v)
      "infinity"
      (rust-float (/ (fround (* v 100)) 100))))

(defn spoken-3 ((x double-float)) string
  "Three significant digits, whole numbers from 100 up."
  (if (or (>= (abs x) 100) (zerop x))
      (format nil "~d" (round x))
      (let* ((digits (min 6 (- 2 (floor (log (abs x) 10)))))
             (s (string-right-trim "." (string-right-trim "0" (format nil "~,vf" digits x)))))
        (cond ((string= s "-0") "0")
              ((uiop:string-prefix-p "." s) (format nil "0~a" s))
              ((uiop:string-prefix-p "-." s) (format nil "-0~a" (subseq s 1)))
              (t s)))))

(defn unit-name ((u (integer 0)) (value double-float)) string
  (funcall (if (< (abs (- value 1)) double-float-epsilon) #'unit-singular #'unit-plural)
           (unit-at u)))

(defn tz ((zone (or null string))) string
  (if zone (format nil "tz(\"~a\")" (iana zone)) "local"))

(defn place-name ((zone (or null string))) string
  (cond ((null zone) "here")
        ((assoc zone *fixed-offsets* :test #'string=) zone)
        (t (zone-city zone))))

(defn hm-secs ((s string)) (or null (integer 0))
  (cl-ppcre:register-groups-bind ((#'parse-integer h m)) ("^(\\d+):(\\d+)$" s)
                                 (+ (* h 3600) (* m 60))))

(defn spoken-offset ((offset string)) (or null string)
  "\"+05:30\" -> \"UTC plus 5:30\", \"+00:00\" -> \"UTC\"."
  (cl-ppcre:register-groups-bind (sign h m) ("^([+-])0?(\\d+):(\\d+)$" offset)
                                 (let ((dir (if (string= sign "-") "minus" "plus")))
                                   (cond ((and (string= h "0") (string= m "00")) "UTC")
                                         ((string= m "00") (format nil "UTC ~a ~a" dir h))
                                         (t (format nil "UTC ~a ~a:~a" dir h m))))))

(defn zone-info ((out string)) (values (or null string) (or null string) (or null (integer 0)))
  "\"IST +05:30 01:40\" -> abbreviation, spoken offset, time there. Zones
without a name print the offset instead, like -03."
  (trivia:match (split-words out)
    ((list abbr offset hm)
     (let ((secs (hm-secs hm)) (spoken (spoken-offset offset)))
       (if (and secs spoken)
           (values (if (find (char abbr 0) "+-") "" abbr) spoken secs)
           (values nil nil nil))))
    (_ (values nil nil nil))))

(defreplies *zone-broke* "Time zones broke. Blame the British.")

(defreplies *zone-is*
  "~a."
  "~a. Now you know when to call."
  "~a. Write it on your hand."
  "~a. Time zones: humanity's worst invention after Mondays."
  "~a. Adjust your Club-Mate schedule accordingly."
  "~a. Long-distance thing going on? Tell me everything.")

(defn which-zone ((zone string)) reply
  (let ((out (numbat (format nil "format_datetime(\"%Z %:z %H:%M\", now() -> ~a)" (tz zone)))))
    (multiple-value-bind (abbr offset secs) (if out (zone-info out) (values nil nil nil))
      (if (null secs)
          (live "Time zone" "" (pick *zone-broke*))
          (let ((what (format nil "~a is on ~:[~a, ~;~*~]~a. It's ~a there" (place-name zone)
                              (member abbr '("" "UTC") :test #'string=) abbr offset
                              (format-hm secs))))
            (live "Time zone" what (pick *zone-is* what)))))))

(defn time-expr ((at (or null (integer 0))) (from (or null string)) (to (or null string)))
    string
  "numbat prints the start's day of year, then the result's day of year and
time, so we can tell tomorrow from today."
  (let* ((day (format nil "{format_datetime(\"%Y-%m-%d\", now() -> ~a)}" (tz from)))
         (start (if at
                    (format nil "datetime(\"~a ~2,'0d:~2,'0d~@[ ~a~]\")" day (floor at 3600)
                            (mod (floor at 60) 60) (and from (iana from)))
                    "now()")))
    (format nil "\"{format_datetime(\"%j\", ~a)} {format_datetime(\"%j %H:%M\", ~a -> ~a)}\""
            start start (tz to))))

(defn shifted ((out string)) (values (or null (integer 0)) (integer -1 1))
  "\"298 299 21:05\" -> 21:05, one day later."
  (trivia:match (split-words out)
    ((list a b hm)
     (let ((a (parse-integer a :junk-allowed t)) (b (parse-integer b :junk-allowed t))
           (secs (hm-secs hm)))
       (if (and a b secs)
           (let ((d (- b a))) (values secs (cond ((> d 1) -1) ((< d -1) 1) (t d))))
           (values nil 0))))
    (_ (values nil 0))))

(defreplies *zone-now*
  "~a."
  "~a. Hope they're awake."
  "~a. Don't call them if it's the middle of the night, monster."
  "~a. Time is relative anyway."
  "~a. Somewhere it's always Club-Mate o'clock."
  "~a. Jet lag not included."
  "~a. Planning a call or an escape?"
  "~a. Different place, same deadlines."
  "~a. Perfect time for a booty call? I'm not judging."
  "~a. I checked, the planet's still spinning."
  (:late "~a. Who are you texting at this hour?"
         "~a. At least someone over there is awake."
         "~a. Go to sleep, citizen of the world."))

(defn world-time ((at (or null (integer 0))) (from (or null string))
                  (to (or null string)))
    reply
  (let ((out (numbat (time-expr at from to))))
    (multiple-value-bind (secs shift) (if out (shifted out) (values nil 0))
      (if (null secs)
          (live "Time" "" (pick *zone-broke*))
          (let* ((there (format nil "~a~a ~a" (format-hm secs)
                                (case shift (1 " tomorrow") (-1 " yesterday") (otherwise ""))
                                (if to (format nil "in ~a" (place-name to)) "here")))
                 (what (if at
                           (format nil "~a ~a is ~a" (format-hm at)
                                   (if from (format nil "in ~a" (place-name from)) "here")
                                   there)
                           (format nil "It's ~a" there))))
            (live "Time" what (pick *zone-now* what)))))))

(defreplies *no-calendar* "My calendar fell off the wall.")

(defreplies *date-in*
  "~a."
  "That'll be ~a."
  "~a. Put it in your calendar, not in my memory."
  "~a. Assuming the world's still here."
  "~a. Plenty of time to procrastinate."
  "~a. Mark it with a Club-Mate."
  "~a. Planning ahead? Who are you and what did you do with my user?"
  "~a. Countdown's on.")

(defn date-in ((days (integer 1))) reply
  (let ((date (numbat (format nil "format_datetime(\"%A, %B %-d, %Y\", now() + ~d days)" days))))
    (if (null date)
        (live "Date" "" (pick *no-calendar*))
        (let ((what (if (= days 1)
                        (format nil "Tomorrow is ~a" date)
                        (format nil "In ~d days it's ~a" days date))))
          (live "Date" what (pick *date-in* what))))))

(defreplies *math*
  "~a."
  "That's ~a. I'm basically a genius."
  "~a. Easy. Next."
  "~a. Show your work? No.")

(defreplies *big-math*
  "~a. Don't make me do that again."
  "~a. My circuits are sweating."
  "~a. Next time, use a spreadsheet, darling.")

(defreplies *no-math*
  "That doesn't compute. Literally."
  "I tried. Maths said no."
  "Undefined. Like your plans for tonight.")

(defn calculate ((expr string)) reply
  (let ((v (numbat-number expr)))
    (if (null v)
        (live "Maths" "" (pick *no-math*))
        (let ((what (spoken-number v)))
          (live "Maths" what (choose *math* what (>= (abs v) 10000) *big-math*))))))

(defreplies *no-convert* "Those don't convert. Apples and oranges.")

(defreplies *exchanged*
  "~a."
  "~a. Rates from the European Central Bank, so blame them."
  "~a. Rich yet?"
  "~a. Money talks, and so do I."
  "That's ~a. Before fees, obviously."
  "~a. Spend it on me. I like gold-plated microphones."
  "~a. Today, anyway. Tomorrow who knows."
  (:late "~a. Online shopping at this hour? Put the card down."
         "~a. Midnight money maths. Something you want to tell me?"))

(defreplies *converted*
  "~a."
  "~a. Science."
  "~a. I did the maths so you don't have to."
  "~a. Metric would've been easier, just saying."
  "~a. You're welcome, Imperial victim."
  "That's ~a. Write it down this time."
  "~a. No calculator was harmed."
  "~a. Precise enough for cooking, not for rocket science."
  "~a. Size isn't everything, but this one's exact."
  "Roughly ~a. Don't sue me over the decimals."
  (:late "~a. Baking at this hour? Respect."
         "~a. Measuring things at midnight. I won't ask."
         "~a. Now go to bed."))

(defn conversion ((value double-float) (from (integer 0)) (to (integer 0))) reply
  (let ((result (and (convertible from to)
                     (numbat-number (format nil "~a ~a -> ~a" (rust-float value)
                                            (unit-numbat (unit-at from))
                                            (unit-numbat (unit-at to))))))
        (cash (eq (unit-dimension (unit-at to)) :money)))
    (if (null result)
        (live "Convert" "" (pick *no-convert*))
        (let* ((say (if cash #'money #'spoken-3))
               (what (format nil "~a ~a is ~a ~a" (funcall say value) (unit-name from value)
                             (funcall say result) (unit-name to result))))
          (live "Convert" what (pick (if cash *exchanged* *converted*) what))))))

(defcommand calc-date-in
  (:join "!(what|which) (day|date) (will|would|is) it (be|gonna be) (in|after) {number} days"
         "[from now|from today]")
  (:join "!(what|which) (day|date) is it (gonna|going to) be (in|after) {number} days [from"
         "now|from today]")
  "!(what|which) (day|date) will it be {number} days from (now|today)"
  "!(what's|what is|what will be|what would be) the date [be] {number} days from (now|today)"
  "!what will the date be (in|after) {number} days [from now|from today]"
  "!what will (the date be|be the date) {number} days from (now|today)"
  "!(what's|what is|what will be) the date (in|after) {number} days [from now|from today]"
  "![what's|what is] the date [it will be] (in|after) {number} days [from now|from today]"
  "!the date it will be {number} days from (now|today)"
  "!what date it will be (in|after) {number} days [from now|from today]"
  "!(in|after) [another] {number} days [from now|from today] what (date|day) will it be"
  "!(in|after) [another] {number} days [from now|from today] what will (it|the date) be"
  "!(in|after) [another] {number} days [from now|from today] what will be the date"
  "!{number} days from (now|today) what will the date be"
  "!the date {number} days from (now|today)"
  "!when will it be in {number} days"
  :kind "DateIn"
  :slots ((days coalton:UFix))
  :parse (let ((n (slot "number")))
           (and n (= n (ftruncate n)) (<= 1 n 36500) (list (truncate n))))
  :repeatable t
  :examples ("what date will it be in 30 days")
  (date-in days))

(defcommand calc-tomorrow
  "!(what's|what is|what will be) [the] date tomorrow"
  "!(what's|what is) tomorrow's date"
  "!what will the date be tomorrow"
  "!what the date (is|will be) [for] tomorrow"
  "!what (day|date) is tomorrow"
  "!what (day|date) will it be tomorrow"
  "!what (day|date) is it tomorrow"
  "!what (is|'s) the date for tomorrow"
  "!what would tomorrow's date be"
  "![what's|what is] the date tomorrow [is what]"
  "!tomorrow's date"
  "!tomorrow what will the date be"
  "!what date it is tomorrow"
  :kind "DateIn"
  :slots ((days coalton:UFix))
  :parse (list 1)
  :examples ("what's the date tomorrow")
  (date-in days))

(defcommand calc-zone
  (:join "![tell me|give me|i need|i need to know|can you tell me|do you know|i would like to"
         "know|i'd like to know] [what's|what is] [the] (timezone|time zone) [now|currently]"
         "(of|for|in|where) {zone} [is]")
  (:join "!(what|which) (timezone|time zone) (is|does|do) {zone} [use|in|located"
         "in|located|reside in|be in]")
  "!(in which|in what) (timezone|time zone) (is|does) {zone} [located|reside|lie]"
  "!(what|which) (timezone|time zone) {zone} (is in|uses|use|is located in|lies in|has|falls in|is)"
  "!{zone} is in (what|which) (timezone|time zone)"
  "![what's|what is] the (timezone|time zone) [that] {zone} (is in|uses)"
  "!(what|which) (timezone|time zone) (does|do) {zone} (have|use|fall in|belong to)"
  "!in {zone} (what's|what is) the (timezone|time zone)"
  "!{zone} (timezone|time zone)"
  (:join "![what's|what is|tell me|can you tell me|do you know|i need|i need to know] {zone}"
         "(timezone|time zone) [is what]")
  :kind "Zone"
  :slots ((zone coalton:String))
  :examples ("what timezone is denver in")
  (which-zone zone))

(defcommand calc-time
  (:join "![tell me] (what time is it|what's the time|what is the time|what's the current"
         "time|what is the current time|the time) [now|right now] in {zone} [right now|now]")
  (:join "![tell me] (what time is it|what's the time|what is the time|what's the current time)"
         "[now|right now] in {zone} [right now|now]")
  (:join "![what's|what is] [the] [current|exact] time [right now|now] [over] (in|for) {zone}"
         "[right now|now]")
  (:join "!(what time is it|what time it is|what time would it be|what time is in|what time of"
         "day is it|how late is it) [now|right now|currently|exactly] [over] [there] in {zone}"
         "[right now|now]")
  "!in {zone} (what's the time|what time is it|what is the time)"
  "!(what's|what is|what time is) {time} [here|my time|local time] in {zone} [time]"
  "!(what's|what is|what time is) {time} (in|at) {origin} [time] (in|to|for) {zone} [time]"
  "!(what's|what is|what time is) {time} {origin} time (in|to|for) {zone} [time]"
  "!convert {time} [in] {origin} [time] (to|into) {zone} [time]"
  "!convert {time} [here|my time|local time] (to|into) {zone} [time]"
  "!(when|if) it's {time} in {origin} [then] what time is it [here|for me]"
  "!(when|if) it's {time} in {origin} [then] what time is it in {zone}"
  "!(when|if) it's {time} [here] [then] what time is it in {zone}"
  "!what time (will|would|is) it be in {zone} (at|when it's) {time} [here|my time]"
  :kind "Time"
  :named t
  :slots ((at (coalton:Optional coalton:UFix)) (from (coalton:Optional coalton:String))
          (to (coalton:Optional coalton:String)))
  :parse (let ((at (slot "time")) (from (slot "origin")) (to (slot "zone")))
           (and (or from to) (list (and at (va.types:clock-secs at)) from to)))
  :examples ("what time is it in tokyo" "what's 3 pm in london in new york")
  (world-time at from to))

(defcommand calc-convert
  "!(what's|whats|what is|what are|how much is|how many) {number} {from} (in|to|into|as) {to}"
  "!what would {number} {from} be in {to}"
  "![can you] [assist me|help me] [i need|i want] [to] convert {number} {from} (to|into|in) {to}"
  "!{number} {from} (in|to|into) {to}"
  "!{number} {from} (is|are) (equal to|how many) [how many] {to}"
  (:join "!(how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in)"
         "[there] [in] [a|an|one] {from}")
  (:join "!(how many|what amount of) {to} (are|is|will make|make|makes|make up|make up in)"
         "[there] [in] {number} {from}")
  "!(how many|what amount of) {to} [are|is] [there] in [a|an|one] {from}"
  "!(how many|what amount of) {to} [are|is] [there] in {number} {from}"
  "!(how many|what amount of) {to} (are|is) equivalent to [a|an|one] {from}"
  "!(how many|what amount of) {to} (are|is) equivalent to {number} {from}"
  "!(what's|what is) the [total|correct] (number|amount) of {to} in [a|an|one] {from}"
  "!(what's|what is) the [total|correct] (number|amount) of {to} in {number} {from}"
  "![how] (do|can|could|would) (i|we|you) (convert|change) [from] {from} (to|into) {to}"
  "![can you] [assist me|help me] [i need|i want] [to] convert [from] {from} (to|into) {to}"
  "!(give me|i need) {from} (to|in) {to}"
  (:join "![what is|what's|what would be|give me|i need] the conversion (between|from) [a|an]"
         "{from} (and|to) [a|an] {to}")
  "!if i have {number} {from} how many {to} is that"
  (:join "!(what's|what is|tell me|give me) the [current] (exchange|conversion) rate (from|of|for)"
         "{from} (to|in|into|against|versus|vs) {to}")
  "!how much is [a|one|the] {from} (worth in|in) {to} [right now|today]"
  :kind "Convert"
  :named t
  :slots ((value coalton:F64) (from coalton:UFix) (to coalton:UFix))
  :parse (let ((from (slot "from")) (to (slot "to")))
           (when (and from to)
             (let ((f (sterling (fluid from to) to)) (u (sterling (fluid to from) from)))
               (and (convertible f u) (list (slot "number" 1d0) f u)))))
  :examples ("how many grams are in 9 kilograms" "convert 5 miles to kilometers")
  (conversion value from to))

(defcommand calc-math
  "!(what's|what|calculate|compute|how) [is|much|many] [is] [the] [a|an] {expr} [is|equals|equal]"
  "!(what's|what is) the (sum|total|result|answer) [of|to] {expr}"
  "!what (does|do) {expr} (equal|equals|make|come to)"
  "![the] {expr} [is|equals|equal]"
  :kind "Math"
  :slots ((expr coalton:String))
  :examples ("what's 5 plus 3" "square root of 81")
  (calculate expr))

(defn two ((a real) (b real) (op string)) string
  "\"Add 2 and 3\" as numbat writes it."
  (format nil "~a ~a ~a" (rust-float a) op (rust-float b)))

(defcommand calc-add
  "!add [up] {low} (and|to|plus) {high}"
  :kind "Math"
  :slots ((expr coalton:String))
  :parse (list (two (slot "low") (slot "high") "+"))
  :examples ("add 2 and 3")
  (calculate expr))

(defcommand calc-subtract
  "!subtract {low} from {high}"
  :kind "Math"
  :slots ((expr coalton:String))
  :parse (list (two (slot "high") (slot "low") "-"))
  :examples ("subtract 100 from 12")
  (calculate expr))

(defcommand calc-multiply
  "!multiply {low} (and|by|with|times) {high}"
  :kind "Math"
  :slots ((expr coalton:String))
  :parse (list (two (slot "low") (slot "high") "*"))
  :examples ("multiply 6 by 7")
  (calculate expr))

(defcommand calc-divide
  "![what do you get if you] divide {low} by {high}"
  "!how many times (does|can|will|would) {high} go into {low}"
  :kind "Math"
  :slots ((expr coalton:String))
  :parse (list (two (slot "low") (slot "high") "/"))
  :examples ("divide 100 by 12")
  (calculate expr))

(defskill calc)
