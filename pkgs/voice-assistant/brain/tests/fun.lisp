;;; The fun skill, ported from the Rust tests.

(in-package #:va-tests)

(define-test fun-phrasings
  :parent va-tests
  (loop for (text want) in
        '(("Tell me a joke." "Fun(Joke)") ("tell a joke." "Fun(Joke)") ("sing for me." "Fun(Sing)")
          ("Tell me a dad joke." "Fun(DadJoke)") ("Heads or tails?" "Fun(Coin)")
          ("Pick a number between 1 and 10" "Fun(Random(1, 10))")
          ("Give me a random number from 100 to 50" "Fun(Random(50, 100))")
          ("Should I order pizza?" "Fun(EightBall)") ("Will I be rich?" "Fun(EightBall)")
          ("Compliment me." "Fun(Compliment)") ("Roast me!" "Fun(Roast)")
          ("I need some motivation." "Fun(Motivate)") ("Tell me a fun fact" "Fun(Fact)")
          ("Sing me a song." "Fun(Sing)"))
        do (is equal want (va::action-debug (va::parse-text text)) "~s" text)))

(define-test listening-is-not-for-the-eight-ball
  :parent va-tests
  (dolist (text '("Can I hear the Beatles?" "Could I listen to some jazz"
                  "can i mix antifreeze with water" "should i hit or stay at 17 in blackjack"
                  "will i be charged if i use my card in russia"))
    (isnt equal "Fun(EightBall)" (va::action-debug (va::parse-text text)) "~s" text)))

(define-test rolls-stay-in-range
  :parent va-tests
  (flet ((body (text) (parse-integer (va::reply-body (va::perform (va::parse-text text))))))
    (loop repeat 50
          do (true (<= 1 (body "roll a die") 6))
          (true (<= -3 (body "pick a number between minus 3 and 3") 3)))))

(define-test jokes-work-offline
  :parent va-tests
  (let ((said nil))
    (is equal '(("curl" "-sfL" "-A" "voice-assistant/0.1.0" "--max-time" "3"
                 "-H" "Accept: text/plain"))
        (mapcar (lambda (e) (subseq e 0 8))
                (subseq (va:fake-effects (setf said (va::perform (va::parse-text "tell a joke"))))
                        0 1)))
    (true (plusp (length (va::reply-speech said))))))
