(in-package #:va-tests)

(define-test expand-spreads-over-every-choice
  :parent va-tests
  (let ((tpl (va::parse-template "(tell|give) [me] a {number} joke")))
    (is equal '("tell a 3 joke" "give a 3 joke" "tell me a 3 joke" "give me a 3 joke")
        (va::expand tpl (lambda (slot) (and (string= slot "number") '("3"))) 40))
    (is = 2 (length (va::expand tpl (lambda (slot) (declare (ignore slot)) '("3"))
                                2)))
    (false (va::expand tpl (constantly nil) 40))))

(define-test only-harmless-commands-are-classified
  :parent va-tests
  (with-focus ()
    (dolist (s '("tell a joke" "what's playing" "how long is left on the timer"
                 "is eve up" "repeat that" "what's on my shopping list"))
      (true (va::classifiable (va::parse-text s)) "~s" s))
    (dolist (s '("turn it up" "connect my headphones" "shut down the computer"
                 "add milk to the shopping list" "join the meeting"))
      (false (va::classifiable (va::parse-text s)) "~s" s))))

(define-test classifier-examples
  :parent va-tests
  (with-focus ()
    (let ((ex (va::classifier-examples)))
      (true (find "another joke" ex :key #'first :test #'string=))
      (is equal '("Timer(Start" t)
          (rest (find "set a timer for five minutes" ex :key #'first :test #'string=)))
      (false (find-if (lambda (e) (search "volume" (first e))) ex) "volume stays grammar-only")
      (false (find "is five minutes on my todo list" ex :key #'first :test #'string=)
             "items take any words, so no made-up ones")
      (is equal ex (remove-duplicates ex :key #'first :test #'string= :from-end t)))))

(define-test fill-finds-slots-anywhere
  :parent va-tests
  (with-focus ()
    (is equal "Timer(Start { seconds: 300, alarm: false, now: 0 })"
        (va::action-debug
         (va::fill-slots "could you start a countdown of five minutes" "Timer(Start")))
    (false (va::fill-slots "could you start a countdown" "Timer(Start"))))

(define-test vetoes
  :parent va-tests
  (true (va::vetoed "Notes(Show" "add eggs to my shopping list and remove cheese"))
  (false (va::vetoed "Notes(Contains" "is cheese on my list"))
  (true (va::vetoed "Info(Holiday" "how much paid leave do i have left"))
  (false (va::vetoed "Info(Holiday" "when is my next holiday"))
  (false (va::vetoed "Fun(Joke)" "add a joke")))

(define-test classified-needs-score-lead-and-slots
  :parent va-tests
  (with-focus ()
    (flet ((c (text kind score lead)
             (let ((r (va::classified text kind score lead)))
               (list (gethash "action" r) (gethash "accepted" r)))))
      (is equal '("Fun(Joke)" t) (c "crack me up with something" "Fun(Joke)" 0.95d0 0.05d0))
      (is equal '("Fun(Joke)" nil) (c "crack me up with something" "Fun(Joke)" 0.80d0 0.05d0))
      (is equal '("Fun(Joke)" nil) (c "crack me up with something" "Fun(Joke)" 0.95d0 0.01d0))
      (is equal '(null nil) (c "start a countdown" "Timer(Start" 0.99d0 0.5d0))
      (is equal '("Notes(Show(0))" nil)
          (c "add eggs and remove cheese" "Notes(Show" 0.99d0 0.5d0)))))
