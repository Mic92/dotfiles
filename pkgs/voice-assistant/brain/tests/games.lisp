;;; Quizzes, riddles and other games that wait for an answer.

(in-package #:va-tests)

(define-test game-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Fun(~a)"
               '(("quiz me" "Trivia") ("ask me a trivia question" "Trivia")
                 ("let's play trivia" "Trivia") ("tell me a riddle" "Riddle")
                 ("give me a riddle" "Riddle") ("would you rather" "WouldYouRather")
                 ("let's play would you rather" "WouldYouRather")
                 ("tell me a bedtime story" "Story") ("tell me a story" "Story")
                 ("sing happy birthday" "BirthdaySong(None)")
                 ("sing happy birthday to shannan" "BirthdaySong(Some(\"shannan\"))")))))

(define-test game-answers
  :parent va-tests
  (true (va::answer-p "it's paris" '("paris")))
  (true (va::answer-p "i think it is mount everest" '("everest")))
  (true (va::answer-p "pariss" '("paris")) "sounds alike")
  (false (va::answer-p "london" '("paris")))
  (false (va::answer-p "" '("paris"))))

(define-test trivia-rounds
  :parent va-tests
  (let ((va::*trivia* '(("What is the capital of France?" "Paris" "paris")))
        (va::*decks* (make-hash-table :test #'eq))
        (va::*fetched-trivia* nil))
    (conversation
     (let ((q (hear "quiz me")))
       (true (search "capital of France" (got q "speech")))
       (true (got q "question")))
     (let ((r (hear "is it paris" "answering" t)))
       (is equal "command" (got r "outcome"))
       (true (search "Paris" (got r "speech")))
       (false (got r "question"))))
    (conversation
     (hear "quiz me")
     (let ((r (hear "berlin" "answering" t)))
       (true (search "Paris" (got r "speech")) "wrong answers learn the right one")))
    (conversation
     (hear "quiz me")
     (is equal "cancel" (got (hear "never mind" "answering" t) "chime")))))

(define-test decks-deal-everything
  :parent va-tests
  (let ((va::*decks* (make-hash-table :test #'eq)))
    (is equal '(1 2 3) (sort (loop repeat 3 collect (va::deal '*x* '(1 2 3))) #'<))))

(defparameter *trivia-api*
  "[{\"correctAnswer\": \"The World Series\",
     \"incorrectAnswers\": [\"Wimbledon\", \"The Hopman Cup\", \"The Americas Cup\"],
     \"question\": {\"text\": \"Which of these is a Baseball competition?\"},
     \"explanation\": \"It began in 1903.\"},
    {\"question\": {\"text\": \"broken\"}}]")

(define-test trivia-from-the-api
  :parent va-tests
  (let ((cards (va::read-trivia *trivia-api*)))
    (is = 1 (length cards))
    (destructuring-bind (question answer explanation accepted positions) (first cards)
      (true (search "Which of these is a Baseball competition?" question))
      (true (search "The World Series" question) "the options are read out")
      (true (search "Wimbledon" question))
      (is equal "The World Series" answer)
      (is equal "It began in 1903." explanation)
      (true (va::answer-p "the world series" accepted))
      (true (va::answer-p "world series" accepted))
      (let* ((options '("The World Series" "Wimbledon" "The Hopman Cup" "The Americas Cup"))
             (order (sort (copy-list options) #'< :key (lambda (o) (search o question))))
             (n (position "The World Series" order :test #'string=)))
        (true (va::said-p (format nil "the ~:r one" (1+ n)) positions))
        (dotimes (other 4)
          (unless (= other n)
            (false (va::said-p (format nil "the ~:r one" (1+ other)) positions))
            (false (va::answer-p (format nil "the ~:r one" (1+ other)) accepted)))))
      (false (va::answer-p "wimbledon" accepted))))
  (is equal '() (va::read-trivia "nope")))
