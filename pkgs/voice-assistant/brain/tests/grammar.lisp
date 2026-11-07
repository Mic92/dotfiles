;;; The template grammar, ported from the Rust tests.

(in-package #:va-tests)

(defun no-slots (name) (declare (ignore name)) nil)
(defun found (template words &optional (parser #'no-slots))
  (multiple-value-bind (skipped slots)
      (va::find-template (va::parse-template template) words parser)
    (and skipped (list skipped slots))))

(define-test phrase-alternatives
  :parent va-tests
  (true (found "(don't|do not) [go to] sleep" '("do" "not" "sleep")))
  (true (found "(don't|do not) [go to] sleep" '("don't" "go" "to" "sleep")))
  (false (found "(don't|do not) [go to] sleep" '("do" "sleep"))))

(define-test optional-slots
  :parent va-tests
  (flet ((days (name)
           (and (string= name "day")
                (lambda (ws next) (declare (ignore next))
                  (and (equal ws '("tomorrow")) 1)))))
    (let ((with (found "!am i busy [{day}]" '("am" "i" "busy" "tomorrow") #'days)))
      (is equal "day" (car (first (second with)))))
    (is equal '(0 nil) (found "!am i busy [{day}]" '("am" "i" "busy") #'days))
    (false (found "!am i busy [{day}]" '("am" "i" "busy" "later") #'days))))

(define-test unanchored-skips-fewest
  :parent va-tests
  (is equal '(2 nil) (found "stop" '("stop" "the" "music")))
  (is equal '(0 nil) (found "stop" '("please" "stop")))
  (false (found "!stop" '("stop" "the" "music"))))

(defun anywhere (template words parser)
  (multiple-value-bind (ok slots) (va::slots-anywhere (va::parse-template template) words parser)
    (and ok (list slots))))

(define-test slots-anywhere
  :parent va-tests
  (flet ((num (name)
           (and (member name '("n" "m") :test #'string=)
                (lambda (ws next) (declare (ignore next)) (and (equal ws '("five")) 5)))))
    (let ((words '("start" "a" "countdown" "of" "five" "please")))
      (is equal '((("n" . 5))) (anywhere "set a timer for {n} [{m}]" words #'num))
      (false (anywhere "{n} and {m}" words #'num))
      (is = 2 (length (first (anywhere "{n} and {m}" '("five" "and" "five") #'num))))
      (false (anywhere "note {text}" words #'num)))))

(define-test variants-change-one-or-two-parts
  :parent va-tests
  (let ((v (va::variants (va::parse-template "(tell|give) [me|us] [a] joke"))))
    (dolist (s '("tell joke" "give joke" "tell a joke" "tell me a joke" "give us joke"))
      (true (member s v :test #'string=) "~s" s)))
  (false (va::variants (va::parse-template "ping {host}"))))

(define-test sentences-for-closed-slots
  :parent va-tests
  (flet ((s (src)
           (va::template-sentences (va::parse-template src)
                                   (lambda (slot)
                                     (and (string= slot "host") '(("eve") ("blob" "64")))))))
    (is equal '("please reach eve" "please reach blob 64")
        (s "!(can you|please) (reach|ping) [the] {host} [now]"))
    (is equal '("ping eve" "ping blob 64") (s "ping {host}"))
    (false (s "say {text}"))
    (false (s "ping {host} and {host}"))
    (false (s "what time is it"))))

(define-test templates-reject-garbage
  :parent va-tests
  (fail (va::parse-template "set {x"))
  (fail (va::parse-template "set (a|b"))
  (fail (va::parse-template "!")))
