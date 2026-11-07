;;; Dispatch, and the golden file written by the Rust `--golden`.

(in-package #:va-tests)

(defun tier-parsed (text)
  "TEXT parsed without the dialogue around it."
  (shown (va::parse-tier text nil)))

(define-test requests-unwrap
  :parent va-tests
  (loop for (text want) in
        '(("Can you tell me what time it is, please" "what time it is")
          ("I need to know what is 85% of 1500" "what is 85 percent of 1500")
          ("please add 456 and 781 for me" "add 456 and 781")
          ("what is 3/4 of 18" "what is 3 over 4 of 18"))
        do (is equal want (format nil "~{~a~^ ~}" (va::unwrap-request (va::normalize text)))
               "~s" text)))

(define-test fun-commands
  :parent va-tests
  (is equal "Fun(Joke)" (tier-parsed "tell me a joke"))
  (is equal "Fun(Dice(6))" (tier-parsed "roll a die"))
  (is equal "Fun(Dice(20))" (tier-parsed "Roll a d20."))
  (is equal "-" (tier-parsed "roll a d 1"))
  (is equal "-" (tier-parsed "blorp")))

(defparameter *rust-zones*
  #("America/New_York" "America/Los_Angeles" "America/Chicago" "America/Denver" "America/Havana"
    "America/Detroit" "America/Los_Angeles" "America/New_York" "America/Boise" "America/Anchorage"
    "America/Bogota" "America/Lima" "America/Guayaquil" "America/Jamaica" "Asia/Phnom_Penh"
    "Asia/Pyongyang" "America/Phoenix" "America/Denver" "America/New_York" "America/Chicago"
    "America/Vancouver" "Asia/Ho_Chi_Minh" "Europe/Madrid" "Pacific/Honolulu" "America/Toronto"
    "America/Vancouver" "America/Mexico_City" "America/Sao_Paulo" "America/Argentina/Buenos_Aires"
    "Europe/London" "Europe/Dublin" "Europe/Lisbon" "Europe/Paris" "Europe/Berlin"
    "Europe/Amsterdam" "Europe/Madrid" "Europe/Rome" "Europe/Vienna" "Europe/Zurich"
    "Europe/Stockholm" "Europe/Helsinki" "Europe/Warsaw" "Europe/Athens" "Europe/Kyiv"
    "Europe/Istanbul" "Europe/Moscow" "Asia/Jerusalem" "Africa/Cairo" "Africa/Johannesburg"
    "Africa/Nairobi" "Africa/Lagos" "Asia/Dubai" "Asia/Kolkata" "Asia/Bangkok" "Asia/Singapore"
    "Asia/Hong_Kong" "Asia/Shanghai" "Asia/Taipei" "Asia/Tokyo" "Asia/Seoul" "Australia/Sydney"
    "Australia/Perth" "Pacific/Auckland" "UTC" "CET" "CEST" "WET" "WEST" "EET" "EEST" "BST" "MSK"
    "EST" "EDT" "CST" "CDT" "MST" "MDT" "PST" "PDT" "AKST" "HST" "JST" "KST" "SGT" "HKT" "AEST"
    "AEDT" "NZST" "NZDT" "IST")
  "What Rust's zone indices are called here, so golden rows compare.")

(defun named-zones (rust)
  "RUST's Calc zone indices as the zone names they stand for."
  (flet ((named (target start end match-start match-end reg-starts reg-ends)
           (declare (ignore start end match-start match-end))
           (let ((n (parse-integer target :start (aref reg-starts 1) :end (aref reg-ends 1))))
             (format nil "~a~s)" (subseq target (aref reg-starts 0) (aref reg-ends 0))
                     (aref *rust-zones* n)))))
    (if (uiop:string-prefix-p "Calc(" rust)
        (cl-ppcre:regex-replace-all "(Zone\\(|from: Some\\(|to: Some\\()(\\d+)\\)" rust #'named)
        rust)))

(defparameter *golden*
  (asdf:system-relative-pathname "voice-assistant-brain" "../tests/golden.tsv"))

(defun golden-rows ()
  (with-open-file (in *golden*)
    (loop for line = (read-line in nil) while line
          unless (uiop:string-prefix-p "#" line)
          collect (destructuring-bind (text rust &rest more)
                      (uiop:split-string line :separator '(#\Tab))
                    (list* text (named-zones rust) more)))))

(defun ported-p (debug)
  "Whether DEBUG names a command the brain has, so it must agree."
  (some (lambda (c)
          (let ((kind (format nil "~a(~a" (va::camel (va:command-skill c))
                              (va::command-kind c))))
            (and (uiop:string-prefix-p kind debug)
                 (find (char debug (length kind)) "() "))))
        (va:commands)))

(defparameter *golden-fixes*
  '(("i don't need eggs anymore, but i do need milk"
     "Notes(Done { items: [\"eggs\"], list: 1 })"
     "Rust kept \"but i do\" as an item")
    ("i don't need grocery shopping on my todo list anymore"
     "Notes(Done { items: [\"grocery shopping\"], list: 0 })"
     "the todo list was taken for part of the item")
    ("i don't need mowing the lawn on my to do list anymore"
     "Notes(Done { items: [\"mowing the lawn\"], list: 0 })"
     "the todo list was taken for part of the item")
    ("i don't need the reservation anymore"
     "Notes(Done { items: [\"reservation\"], list: 1 })"
     "anymore is no part of the item")
    ("i don't need toilet paper anymore; take it off my shopping list"
     "Notes(Done { items: [\"toilet paper\"], list: 1 })"
     "what follows the item is an instruction"))
  "Sentences the brain reads better than Rust: (text what-it-says-now why).")

(define-test golden
  :parent va-tests
  (let ((va::*context* (va::make-context (* 9 3600) (va::day-number 2025 10 22))))
    (loop for (text rust) in (golden-rows)
          for lisp = (tier-parsed text)
          for fix = (assoc text *golden-fixes* :test #'string=)
          do (when fix (setf rust (second fix)))
          ;; Other skills may still win once they are ported.
          when (or (ported-p rust) (string= rust "-"))
          ;; What Say says is picked at random.
          do (if (uiop:string-prefix-p "Say(" rust)
                 (true (uiop:string-prefix-p "Say(" lisp) "~s: ~a" text lisp)
                 (is equal rust lisp "~s" text)))))

(define-test http-user-agent
  :parent va-tests
  (let ((ran (va:fake-effects (va::http-get "https://example.com"))))
    (is equal "voice-assistant/0.1.0" (second (member "-A" (first ran) :test #'equal)))))
