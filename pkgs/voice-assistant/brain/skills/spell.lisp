;;; "How do you spell potato?", letter by letter.

(in-package #:va)

(in-skill spell)

(defparameter *not-word* '("it" "that" "this" "me" "something" "words" "word" "out")
  "Pronouns and fillers that are not the word to spell, as in \"spell it\".")

(defn spell-word ((ws words)) (or null string)
  "One plain word as heard. ASR picks the spelling, so that is all we offer."
  (trivia:match ws
    ((list w) (and (not (member w *not-word* :test #'string=)) (every #'alpha-char-p w) w))))

(defn spell-letter ((ws words)) (or null character)
  "\"r\", \"r's\", \"rs\", \"letter r\"."
  (cl-ppcre:register-groups-bind (c) ("^(?:the )?(?:letter )?([a-z])(?:'?s)?$" (unwords ws))
                                 (char c 0)))

(defslot "word" (ws next) (spell-word ws))
(defslot "letter" (ws next) (spell-letter ws))

(defn letters ((word string)) string
  "\"P, O, T, A, T, O\", with \"double S\" for a repeated letter."
  (format nil "~{~a~^, ~}"
          (mapcar (lambda (m) (if (= (length m) 2) (format nil "double ~a" (char m 0)) m))
                  (cl-ppcre:all-matches-as-strings "(.)\\1?" (string-upcase word)))))

(defn letter-count ((letter (or null character)) (word string)) string
  (if (null letter)
      (format nil "~a has ~d letters" word (length word))
      (let ((n (count letter word :test #'char-equal)) (name (char-upcase letter)))
        (case n
          (0 (format nil "There's no ~a in ~a" name word))
          (1 (format nil "~a has one ~a" word name))
          (otherwise (format nil "~a has ~d ~a's" word n name))))))

(defreplies *spelled*
  "~a."
  "~a. Spelling bee champion, right here."
  "~a. Write it down, I won't spell it twice. Okay, I would."
  "~a. You're welcome, autocorrect."
  "~a. Easy. Ask me a hard one."
  "~a. Your English teacher would be proud of me."
  "~a. Sexy letters, all of them."
  "~a. I've got a way with letters. And with you."
  "~a. Don't tell spell check I did its job."
  "~a. Now use it in a sentence, sweetie."
  (:late "~a. Spelling at this hour? Writing a love letter?"
         "~a. Even sleepy, I spell better than you."
         "~a. Now go to bed, word nerd."))

(defreplies *spelled-long*
  "~a. Phew. Who even uses that word?"
  "~a. That one deserves a Club-Mate."
  "~a. Show-off.")

(defreplies *counted*
  "~a."
  "~a. I counted twice, just for you."
  "~a. Maths and spelling, I'm the full package."
  "~a. Next question, smarty pants."
  "~a. Counting letters, how thrilling.")

(defcommand spell
  (:join "!how (do|would|should|can|could) (you|i|one) [correctly|properly] spell [out]"
         "[the word] {word}")
  "!how to [correctly|properly] spell [out] [the word] {word}"
  (:join "![tell me|show me|i wish i knew|remind me|i don't know|i do not know|i'm not sure"
         "|i am not sure|i forget|i forgot|i can't remember|i can't figure out]"
         "how to [correctly|properly] spell [out] [the word] {word}")
  (:join "!how (is|do you spell|does one spell|does the dictionary spell)"
         "[the word] {word} [spelled|spelt]")
  "!how [the word] {word} is (spelled|spelt)"
  "![the word] {word} is (spelled|spelt) how [exactly]"
  (:join "!(what's|what is) the [correct|right|proper|complete] spelling (of|for)"
         "[the word] {word}")
  (:join "!(give|tell) me (the|a) [correct|right|proper|complete] spelling (of|for)"
         "[the word] {word}")
  "!can you give me (the|a) [correct|right|proper] spelling (of|for) [the word] {word}"
  "![the] [correct|right|proper|complete] spelling (of|for) [the word] {word}"
  (:join "!(what's|what is) the (correct|right|proper) way (to spell|of spelling)"
         "[the word] {word}")
  "!how would i go about spelling [the word] {word}"
  "![i need|can you|could you] help [me] [with] (spelling|spell) [the word] {word}"
  "!i can't figure out how to spell [the word] {word}"
  "![can you|could you|would you|please] spell [out] [the word] {word} [for me]"
  :kind "Word"
  :slots ((word coalton:String))
  :repeatable t
  :examples ("spell potato" "how do you spell tomato")
  (let ((what (format nil "~a. ~a" word (letters word))))
    (live "Spelling" what (choose *spelled* what (>= (length word) 11) *spelled-long*))))

(defcommand spell-count
  "!how many {letter} (are|is) [there] in [the word] {word}"
  "!how many {letter} can you find in [the word] {word}"
  "!count [the number of] {letter} in [the word] {word}"
  "!how many {letter} (does|do) [the word] {word} have"
  "!how many letters (are|is) [there] in [the word] {word}"
  "!how many letters (does|do) [the word] {word} have"
  "![the word] {word} (has|got|have) how many letters"
  :kind "Count"
  :named t
  :slots ((letter (coalton:Optional coalton:Char)) (word coalton:String))
  :repeatable t
  :examples ("how many r's are in restaurant" "the word you got how many letters")
  (let ((what (letter-count letter word)))
    (live "Spelling" what (pick *counted* what))))

(defskill spell)
