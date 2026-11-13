;;; Maths and unit conversion, worked out by numbat.

(in-package #:va)

(in-skill calc)

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
