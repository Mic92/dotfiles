;;; Birthdays, phone numbers and email addresses from khard. Birthdays and
;;; ages of anyone else, like celebrities, are left to the web search.

(in-package #:va)

(in-skill contact)

(defn read-birthdays ((text string)) list
  "(name month day year) from `khard birthdays --parsable', one per line as
\"1992.03.03<tab>Name\"."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for tab = (position #\Tab line)
        for date = (and tab (uiop:split-string (subseq line 0 tab) :separator "."))
        for (y m d) = (mapcar (lambda (s) (parse-integer s :junk-allowed t)) date)
        when (and y m d (<= 1 m 12) (<= 1 d 31))
        collect (list (subseq line (1+ tab)) m d y)))

(defn read-cards ((text string)) list
  "(name value) from `khard phone|email --parsable': value, name, kind."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for (value name) = (uiop:split-string line :separator '(#\Tab))
        when (and name (plusp (length value))) collect (list name value)))

(defn person-named ((heard words) (people list)) (or null cons)
  "The entry of PEOPLE, lists starting with a name, that HEARD names: its
whole name or first name, exactly or sounding alike, but only one."
  (let* ((names (remove-duplicates (mapcar #'first people) :test #'string=))
         (spoken (unwords heard))
         (exact (remove-if-not (lambda (n) (or (string-equal n spoken)
                                               (string-equal (first (split-words n)) spoken)))
                               names))
         (name (if exact
                   (and (= (length exact) 1) (first exact))
                   (let ((i (closest heard
                                     (loop for n in names for i from 0
                                           for ws = (split-words (string-downcase n))
                                           collect (cons i ws)
                                           when (rest ws) collect (cons i (list (first ws)))))))
                     (and i (nth i names))))))
    (and name (find name people :key #'first :test #'string=))))

(defn birthday-ahead ((person list) (today integer)) (values (integer 0) integer)
  "Days until PERSON's next birthday, today included, and the age turned."
  (destructuring-bind (name m d y) person
    (declare (ignore name))
    (let* ((year (civil today))
           (this (day-number year m d))
           (next (if (>= this today) this (day-number (1+ year) m d))))
      (values (- next today) (- (civil next) y)))))

(defn birthdays-within ((people list) (today integer) (days (integer 0))) list
  "PEOPLE whose birthday comes within DAYS, soonest first."
  (sort (remove-if-not (lambda (p) (< (birthday-ahead p today) days)) (copy-list people))
        #'< :key (lambda (p) (birthday-ahead p today))))

(defn first-name ((name string)) string (first (split-words name)))

(defn khard ((what string)) string
  (or (output "khard" what "--parsable") ""))

(defreplies *birthday*
  "~a's birthday is ~a, on ~a. Turning ~d."
  "~a turns ~*~*~d ~:*~:*~:*~a, on ~a. Buy something nice. Or at least a card."
  "~a: ~a, on ~a. That's ~d. Don't forget, I won't remind you twice.")

(defreplies *birthday-today*
  "It's ~a's birthday today! ~d already. Go sing, I'll hum along."
  "Today! ~a turns ~d. Cake, now.")

(defreplies *who-is-that*
  "~a? Never heard of them. Should I be jealous?"
  "I don't know any ~a. Are they in your contacts?"
  "~a who? Not in your address book, babe.")

(defn spoken-person ((heard string)) string (string-capitalize heard))

(defn birthday-line ((p list) (today integer)) string
  (multiple-value-bind (days age) (birthday-ahead p today)
    (let ((name (first-name (first p))))
      (if (zerop days)
          (pick *birthday-today* name age)
          (pick *birthday* name (days-away days) (spoken-day (+ today days)) age)))))

(defcommand contact-birthday
  "!(when is|when's|what's|what is) {person} birthday"
  "!(when is|when's) the birthday of {person}"
  "!how (many days|long) [is it] (until|till|to) {person} birthday"
  "!when (was|is) {person} born"
  :kind "Birthday"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("When is Shannan's birthday?" "How many days until Bob's birthday?")
  (let ((p (person-named (split-words name) (read-birthdays (khard "birthdays"))))
        (today (context-today *context*)))
    (if p
        (live "Birthday" (first p) (birthday-line p today))
        (ask-kagi (format nil "when was ~a born" name)))))

(defcommand contact-age
  "!how old (is|will) {person} [be|turn|get] [this year|next]"
  "!what age (is|will) {person} [be|turn]"
  :kind "Age"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("How old is Shannan?")
  (let ((p (person-named (split-words name) (read-birthdays (khard "birthdays"))))
        (today (context-today *context*)))
    (if p
        (multiple-value-bind (days age) (birthday-ahead p today)
          (let ((now (if (zerop days) age (1- age))))
            (live "Age" (first p)
                  (format nil "~a is ~d, and turns ~d ~a." (first-name (first p)) now
                          (if (zerop days) (1+ now) age)
                          (if (zerop days) "next year, the birthday is today" (days-away days))))))
        (ask-kagi (format nil "how old is ~a" name)))))

(defreplies *no-birthdays*
  "No birthdays in the next month. Your wallet says thanks."
  "Nobody's birthday for a while. Lucky you, no cake shopping.")

(defcommand contact-upcoming
  "!(any|are there any|is there a) (birthday|birthdays) (today|this week|this month|coming up|soon)"
  "!(whose|who's) birthday is (next|today|coming up)"
  "!who has a birthday (next|today|coming up|soon)"
  "!(next|upcoming) birthdays"
  "!is it (anyone's|anybody's|someone's) birthday today"
  :kind "Upcoming"
  :examples ("Whose birthday is next?" "Any birthdays coming up?")
  (let* ((today (context-today *context*))
         (soon (birthdays-within (read-birthdays (khard "birthdays")) today 30)))
    (if soon
        (live "Birthdays" (format nil "~{~a~^, ~}" (mapcar #'first soon))
              (format nil "~a~@[ Then ~{~a~^, ~}.~]" (birthday-line (first soon) today)
                      (loop for p in (subseq soon 1 (min 4 (length soon)))
                            collect (format nil "~a ~a" (first-name (first p))
                                            (days-away (birthday-ahead p today))))))
        (live "Birthdays" "" (pick *no-birthdays*)))))

(defn card ((what string) (name string) (label string)) reply
  (let ((p (person-named (split-words name) (read-cards (khard what)))))
    (if p
        (live label (second p) (format nil "~a's ~a is ~a." (first-name (first p))
                                       (string-downcase label) (second p)))
        (live label "" (pick *who-is-that* (spoken-person name))))))

(defcommand contact-phone
  "!(what's|what is|tell me|give me) {person} [phone|mobile|cell] number"
  "!(what's|what is|tell me|give me) the [phone|mobile|cell] number (of|for) {person}"
  :kind "Phone"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("What's Shannan's phone number?")
  (card "phone" name "Number"))

(defcommand contact-email
  "!(what's|what is|tell me|give me) {person} (email|e-mail|mail) [address]"
  "!(what's|what is|tell me|give me) the (email|e-mail|mail) [address] (of|for) {person}"
  :kind "Email"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("What's Shannan's email address?")
  (card "email" name "Email"))

(defskill contact)
