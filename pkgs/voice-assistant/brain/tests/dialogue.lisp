;;; Dialogue acts, ported from the Rust tests.

(in-package #:va-tests)

(defmacro with-focus ((&key timer music previous) &body body)
  `(let ((va::*context* (va::make-context 0 (va::day-number 2025 10 22)
                                          :timer ,timer :music ,music :previous ,previous)))
     ,@body))

(defun shown (action) (va::action-debug action))

(define-test ambient-acts
  :parent va-tests
  (loop for (act timer music want) in
        '((:stop t t "Timer(Cancel)") (:cancel t t "Timer(Cancel)") (:no t nil "Dismiss")
          (:cancel nil t "Dismiss") (:stop nil nil "Dismiss") (:pause t t "Timer(Pause)")
          (:yes nil nil "-"))
        do (is equal want (with-focus (:timer timer :music music) (shown (va::ambient act)))
               "~s timer ~s music ~s" act timer music)))

(define-test dialogue-phrasings
  :parent va-tests
  (loop for (text want) in
        '(("Stop!" "Timer(Cancel)") ("stop it now!" "Timer(Cancel)") ("Cancel." "Timer(Cancel)")
          ("Alexa, cancel." "Timer(Cancel)") ("Cancel it, please." "Timer(Cancel)")
          ("oops cancel that" "Timer(Cancel)") ("please cancel that last command" "Timer(Cancel)")
          ("abort mission" "Timer(Cancel)") ("Cancel the timer." "Timer(Cancel)")
          ("Pause." "Timer(Pause)") ("never mind" "Dismiss") ("Alexa, forget it." "Dismiss")
          ("never mind, don't do that" "Dismiss")
          ("actually forget that i don't need it" "Dismiss")
          ("forget what i told you to do" "Dismiss") ("stop, i changed my mind" "Dismiss")
          ("Nothing." "Dismiss"))
        do (is equal want (with-focus (:timer t) (shown (va::parse-text text))) "~s" text))
  (is equal "Dismiss" (with-focus () (shown (va::parse-text "Stop!")))))

(define-test questions-come-first
  :parent va-tests
  (with-focus (:timer t :music t)
    (let ((q (va::confirm (va::command-action 'va::nap 1) (va::say-action "phew"))))
      (flet ((ask (text) (shown (va::respond text q))))
        (dolist (text '("Yes." "Yeah, do it" "Do it!" "sure thing" "of course"))
          (is equal "Nap(1)" (ask text) "~s" text))
        (dolist (text '("No, wait!" "Please don't." "nah" "forget it" "cancel that" "stop"
                        "i'll pass"))
          (is equal "Say(\"phew\")" (ask text) "~s" text))
        (let ((unsure (ask "i don't know")))
          (true (and (uiop:string-prefix-p "Say(" unsure) (string/= unsure "Say(\"phew\")"))))))
    (is equal "Timer(Cancel)" (shown (va::respond "stop" nil)))
    (is equal "-" (shown (va::respond "yes" nil)))))

(define-test unhandled-acts-dismiss-the-question
  :parent va-tests
  (with-focus (:timer t)
    (is equal "Dismiss" (shown (va::respond "stop" (va::question))))))

(define-test answer-words
  :parent va-tests
  (flet ((a (text) (va::answer-word (va::normalize text))))
    (false (a "What?"))
    (loop for (text want) in
          '(("no idea" :maybe) ("i don't know" :maybe) ("um, i'm not quite sure" :maybe)
            ("that is not false" :yes) ("you are not wrong about that" :yes)
            ("yes, that is correct" :yes) ("roger that" :yes) ("false for sure" :no)
            ("that isn't correct" :no) ("certainly not" :no) ("i think not" :no)
            ("please, no" :no) ("i do not think that is true" :no))
          do (is eq want (a text) "~s" text))
    (false (a "well I was going to say something else entirely here"))))
