;;; Dispatch, and the golden file of sentences.

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

(defparameter *golden*
  (asdf:system-relative-pathname "voice-assistant-brain" "../tests/golden.tsv")
  "Sentences and what the grammar reads in them: real requests from CLINC
and the logs, close misses that must stay misses, and a few far ones.")

(defun golden-rows ()
  (with-open-file (in *golden*)
    (loop for line = (read-line in nil) while line
          unless (uiop:string-prefix-p "#" line)
          collect (uiop:split-string line :separator '(#\Tab)))))

(define-test golden
  :parent va-tests
  (let ((va::*context* (va::make-context (* 9 3600) (va::day-number 2025 10 22))))
    (loop for (text want) in (golden-rows)
          for got = (tier-parsed text)
          ;; What Say says is picked at random.
          do (if (uiop:string-prefix-p "Say(" want)
                 (true (uiop:string-prefix-p "Say(" got) "~s: ~a" text got)
                 (is equal want got "~s" text)))))

(define-test http-user-agent
  :parent va-tests
  (let ((ran (va:fake-effects (va::http-get "https://example.com"))))
    (is equal "voice-assistant/0.1.0" (second (member "-A" (first ran) :test #'equal)))))
