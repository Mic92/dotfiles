;;; Yes, no or maybe in a loosely worded answer to a question.

(in-package #:va)

(defparameter *negators*
  '("not" "never" "don't" "doesn't" "isn't" "aren't" "can't" "couldn't" "haven't" "dont" "isnt"
    "cant" "unable" "wasn't" "don’t" "isn’t")
  "Words turning an answer around, as in \"I don't know\".")

(defparameter *knowing*
  '("know" "sure" "certain" "confident" "say" "tell" "decide" "information" "mind" "answer"
    "idea" "clue")
  "What a negator denies in an unsure answer, as in \"no idea\".")

(defparameter *maybe*
  '("maybe" "perhaps" "dunno" "idk" "possibly" "unsure" "undecided" "uncertain" "might" "either"
    "both" "unaware" "possibility" "foggiest" "depends" "knows")
  "Words of an unsure answer.")

(defparameter *true*
  '("true" "right" "correct" "accurate" "fact" "facts" "valid" "truth")
  "Agreeing with a statement, which answers yes.")

(defparameter *false*
  '("false" "wrong" "incorrect" "inaccurate" "untrue" "erroneous" "invalid" "fake")
  "Disagreeing with a statement, which answers no.")

(defparameter *yes*
  '("yes" "yeah" "yep" "yup" "yeap" "ya" "confirm" "confirmed" "affirmative" "affirmitive"
    "agree" "agreed" "accepted" "approved" "roger" "indeed" "positive")
  "Words that answer yes.")

(defparameter *no*
  '("no" "nope" "nah" "naw" "nay" "cancel" "abort" "wait" "pass" "negative" "negatory" "nada"
    "disagree")
  "Words that answer no.")

(defparameter *leans-yes*
  '("sure" "absolutely" "definitely" "certainly" "please" "ok" "okay" "great" "awesome")
  "Words that answer yes only when nothing says otherwise.")

(defparameter *maybe-pairs*
  '(("no" "idea") ("no" "clue") ("a" "clue") ("no" "knowledge") ("could" "be") ("if" "i"))
  "Word pairs of an unsure answer.")

(defparameter *yes-pairs*
  '(("do" "it") ("do" "that") ("go" "ahead") ("of" "course") ("uh" "huh") ("all" "right")
    ("got" "it") ("checks" "out") ("makes" "sense"))
  "Word pairs that answer yes.")

(defn has-pair ((ws words) (pairs list)) boolean
  (loop for (a b) on ws thereis (and b (member (list a b) pairs :test #'equal) t)))

(defn answer-word ((ws words)) (member nil :yes :no :maybe)
  "Yes, no or maybe as an answer to a question. Loose on purpose, so only
used while one is pending. Longer sentences are about something else."
  (when (> (length ws) 12) (return-from answer-word nil))
  (when (or (intersection ws *maybe* :test #'string=) (has-pair ws *maybe-pairs*))
    (return-from answer-word :maybe))
  (let* ((strong (has-pair ws *yes-pairs*))
         (score (if strong 2 0))
         (truth nil)
         ;; A negator flips the next truth or knowing word only.
         (negated nil))
    (dolist (w ws)
      (flet ((one-of (list) (member w list :test #'string=)))
        (cond ((one-of *negators*) (setf negated t))
              ((string= w ",") (setf negated nil))
              ((and negated (one-of *knowing*)) (return-from answer-word :maybe))
              ((or (one-of *true*) (one-of *false*))
               (let ((sign (if (one-of *true*) 2 -2)))
                 (incf score (if negated (- sign) sign))
                 (setf truth t strong t negated nil)))
              ((or (one-of *yes*) (one-of *no*))
               (incf score (if (one-of *yes*) 2 -2))
               (setf strong t))
              ((one-of *leans-yes*) (incf score)))))
    ;; "please don't", "i think not": a negator with nothing to flip.
    (when (and (not truth) (intersection ws *negators* :test #'string=))
      (decf score 3)
      (setf strong t))
    ;; "sure", "okay please" are answers, a sentence with a "please" is not.
    (cond ((and (not strong) (> (length ws) 3)) nil)
          ((plusp score) :yes)
          ((minusp score) :no))))
