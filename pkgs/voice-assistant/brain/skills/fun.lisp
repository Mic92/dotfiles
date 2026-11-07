(in-package #:va)

(in-skill fun)

(defreplies *joke-intros*
  "Let me think of a good one."
  "Oh, I've got one."
  "Hang on, I'm rummaging through my joke drawer."
  "Okay. Brace yourself."
  "Fine, but you have to laugh this time."
  "One sec, picking one my lawyer approved."
  "Right. Lower your expectations."
  "I've been saving this one for you."
  "Drum roll, please."
  "Okay, try not to snort your Club-Mate.")

(defreplies *coin*
  "~a! No take-backs.")

(defcommand joke
  "[please] (tell|give) [me|us] [a|another|one more] [funny|dirty|bad|good|short] joke [please]"
  "(make|cheer) (me|us) (laugh|up)"
  "![another|one more] joke [please]"
  :filler *joke-intros*
  :repeatable t
  :examples ("Tell me a joke." "Tell me another dirty joke" "Joke!")
  "Why do Lisp programmers never finish? They keep adding parentheses.")

(defcommand coin
  "(flip|toss) a coin" "heads or tails" "!coin (flip|toss)"
  :repeatable t
  :examples ("flip a coin" "heads or tails")
  (pick *coin* (if (zerop (random 2)) "Heads" "Tails")))

(defcommand dice
  "roll a (die|dice|d6)" "roll [the] dice" "roll a d {number}" "roll a {number} sided (die|dice)"
  :slots ((sides coalton:UFix))
  :repeatable t
  :examples ("roll a die" "roll a d 20")
  (let ((roll (1+ (random sides))))
    (cond ((= roll 1) "A one. Ouch. Critical fail.")
          ((= roll sides) (format nil "~d! Natural max, baby!" roll))
          (t (format nil "You rolled a ~d." roll)))))

(defskill fun)
