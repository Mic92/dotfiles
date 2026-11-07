;;; Dispatch, and the golden file written by the Rust `--golden`.

(in-package #:va-tests)

(defun parsed (text) (va::action-debug (va::parse-tier text nil)))

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
  (is equal "Fun(Joke)" (parsed "tell me a joke"))
  (is equal "Fun(Dice(6))" (parsed "roll a die"))
  (is equal "Fun(Dice(20))" (parsed "Roll a d20."))
  (is equal "-" (parsed "roll a d 1"))
  (is equal "-" (parsed "blorp")))

(defparameter *golden*
  (asdf:system-relative-pathname "voice-assistant-brain" "../tests/golden.tsv"))

(defun golden-rows ()
  (with-open-file (in *golden*)
    (loop for line = (read-line in nil) while line
          unless (uiop:string-prefix-p "#" line)
          collect (uiop:split-string line :separator '(#\Tab)))))

(defun ported-p (debug)
  "Whether DEBUG names a command the brain has, so it must agree."
  (some (lambda (c)
          (let ((kind (format nil "~a(~a" (va::camel (va:command-skill c))
                              (va::command-kind c))))
            (and (uiop:string-prefix-p kind debug)
                 (find (char debug (length kind)) "()"))))
        (va:commands)))

(define-test golden
  :parent va-tests
  (let ((va::*context* (va::make-context (* 9 3600) (va::day-number 2025 10 22))))
    (loop for (text rust) in (golden-rows)
          for lisp = (parsed text)
          ;; Other skills may still win once they are ported.
          when (or (ported-p rust) (string= rust "-"))
          do (is equal rust lisp "~s" text))))
