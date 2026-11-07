;;; Kagi, ported from the Rust tests.

(in-package #:va-tests)

(defparameter *blaze*
  (format nil "The phrase \"**420 blaze it**\" is slang [^2][^3]. Its meaning has two parts:~%~%~
- **420**: A code term for cannabis [^3]. It also means 4:20 p.m. on April 20th.~%~
- **Blaze it**: Slang for smoking [^5].~%~%## Usage and Context~%~
It is used ironically, e.g. in memes.~%~%[^2]: https://example.com"))

(define-test kagi-spoken
  :parent va-tests
  (is equal (concatenate 'string "The phrase \"420 blaze it\" is slang. Its meaning has two parts: "
                         "420: A code term for cannabis. Blaze it: Slang for smoking.")
      (va::spoken *blaze* 3))
  (is equal "It is used ironically, e.g. in memes."
      (va::spoken "It is used ironically, e.g. in memes. Really." 1))
  (is equal "" (va::spoken "" 2)))

(define-test kagi-phrasings
  :parent va-tests
  (with-focus ()
    (loop for (text q) in
          '(("can you tell me how to clean a fish tank" "how to clean a fish tank")
            ("tell me who sells dixie paper plates" "who sells dixie paper plates")
            ("find out why the sky is blue" "why the sky is blue"))
          do (is equal (format nil "Kagi(Query(~s))" q) (shown (va::parse-text text)) "~s" text))
    (loop for (text q) in
          '(("Search for the best pizza in Munich" "the best pizza in munich")
            ("Look up nix flakes" "nix flakes") ("Ask Kagi about black holes" "black holes")
            ("Who was Ada Lovelace?" "who was ada lovelace")
            ("How tall is the Eiffel tower?" "how tall is the eiffel tower")
            ("What is a quasar?" "what is a quasar")
            ("How many people live in Germany?" "how many people live in germany"))
          do (is equal (format nil "Kagi(Query(~s))" q) (shown (va::parse-text text)) "~s" text))
    (dolist (text '("What's the time, please?" "How much time is left on the timer?"
                    "What time is it?" "What is the weather like?"))
      (false (uiop:string-prefix-p "Kagi" (shown (va::parse-text text))) "~s" text))
    (false (funcall (va::slot-parser "question") '("what" "now") nil))))

(define-test kagi-answer
  :parent va-tests
  (multiple-value-bind (md url)
      (va::parse-answer (concatenate 'string "{\"results\": [], \"quick_answer\": {\"markdown\": "
                                     "\"The **Eiffel Tower** stands at **330 metres** [^2][^3]. "
                                     "It was built in 1889 [^1]. Third sentence."
                                     "\\n\\n- **Detail**: more\", \"references\": "
                                     "[{\"title\": \"B\", "
                                     "\"url\": \"https://britannica.com/x\"}]}}"))
    (is equal "https://britannica.com/x" url)
    (is equal "The Eiffel Tower stands at 330 metres. It was built in 1889." (va::spoken md 2)))
  (false (va::parse-answer "{\"quick_answer\": {\"markdown\": \"\"}}")))
