;;; Template grammar for intents, in the spirit of Home Assistant's hassil.
;;;
;;; A template is a sequence of
;;;   word      literal
;;;   (a|b)     one of several words
;;;   [a|b]     optional word
;;;   {slot}    one or more words accepted by the slot's parser
;;; A leading ! anchors it: apart from fillers ("alexa", "please") the
;;; utterance must be the template alone. Unanchored templates may be
;;; surrounded by other words. The match ignoring the fewest non-filler
;;; words wins, so "stop the music" beats a generic "stop".

(in-package #:va)

(defparameter *open-slots* '("text" "items" "any" "place" "person")
  "Slots taking arbitrary words, which only a full match can delimit.")

(defparameter *fillers*
  '("alexa" "please" "hey" "ok" "okay" "um" "uh" "so" "now" "just" "could" "can"
    "would" "you" "thanks" ",")
  "Words skipped between template words.")

(deftype slot-parser ()
  "Words a slot would take and the word after them, to a value or nil."
  '(function (list (or null string)) t))

(deftype slots () "Slot name to value." 'list)

(defvar *cue-words* (make-hash-table :test #'equal :synchronized t)
  "Every template's cue words, one string each, so cues compare with eq.")

(defn cue-word ((w string)) string
  (or (gethash w *cue-words*) (setf (gethash w *cue-words*) w)))

(defn part-cues ((parts list)) list
  (loop for (kind alts) in parts
        when (eq kind :word) collect (mapcar (lambda (alt) (cue-word (first alt))) alts)))

(defstruct (template (:constructor %make-template
                                   (parts anchored source &aux (cues (part-cues parts)))))
  "A parsed template. Each part is (:word alts), (:optional alts) or (:slot
name optional), alts being lists of words, longest first. CUES has the
first words of each required part's alternatives: without one of each in
a sentence the template can't match."
  (parts nil :type list)
  (anchored nil :type boolean)
  (source "" :type string)
  (cues nil :type list))

(defn template-tokens ((src string)) list
  "SRC split into tokens, bracketed groups whole."
  (let ((out nil) (i 0) (n (length src)))
    (loop
     (loop while (and (< i n) (char= (char src i) #\Space)) do (incf i))
     (when (>= i n) (return (nreverse out)))
     (let* ((close (case (char src i) (#\( #\)) (#\[ #\]) (#\{ #\}) (otherwise nil)))
            (end (if close
                     (let ((e (position close src :start i)))
                       (unless e (bad-template "unclosed group in template ~s" src))
                       (1+ e))
                     (or (position #\Space src :start i) n))))
       (push (subseq src i end) out)
       (setf i end)))))

(define-condition template-error (error)
  ((message :initarg :message))
  (:report (lambda (c s) (write-string (slot-value c 'message) s))))

(defn bad-template ((control string) &rest (args t)) nil
  (error 'template-error :message (apply #'format nil control args)))

(defn alternatives ((s string)) list
  "\"a|do not\" as word lists, longest first, so \"(another|another one)\" does
not stop after \"another\"."
  (stable-sort (mapcar #'split-words
                       (uiop:split-string s :separator "|"))
               #'> :key #'length))

(defn inside ((tok string) (open string) (close string)) (or null string)
  "TOK without OPEN and CLOSE around it, or nil."
  (and (>= (length tok) (+ (length open) (length close)))
       (uiop:string-prefix-p open tok)
       (uiop:string-suffix-p tok close)
       (subseq tok (length open) (- (length tok) (length close)))))

(defn template-part ((tok string)) list
  (let (inner)
    (cond ((setf inner (inside tok "(" ")")) (list :word (alternatives inner)))
          ((setf inner (inside tok "[{" "}]")) (list :slot inner t))
          ((setf inner (inside tok "[" "]")) (list :optional (alternatives inner)))
          ((setf inner (inside tok "{" "}")) (list :slot inner nil))
          ((every (lambda (c) (or (alphanumericp c) (char= c #\'))) tok)
           (list :word (list (list tok))))
          (t (bad-template "bad token ~s" tok)))))

(defn parse-template ((src string)) template
  (let* ((anchored (uiop:string-prefix-p "!" src))
         (body (if anchored (subseq src 1) src))
         (parts (mapcar #'template-part (template-tokens body))))
    (unless parts (bad-template "empty template ~s" src))
    (%make-template parts anchored src)))

(defn skip-commas ((words simple-vector) (wi fixnum)) fixnum
  (loop while (and (< wi (length words)) (comma-p (svref words wi))) do (incf wi))
  wi)

(defn slot-names ((template template)) list
  "(name . optional) for each slot, in order."
  (loop for (kind name optional) in (template-parts template)
        when (eq kind :slot) collect (cons name optional)))

(defn has-slots ((template template)) boolean
  (and (slot-names template) t))

(defn has-open-slots ((template template)) boolean
  (and (intersection (mapcar #'car (slot-names template)) *open-slots* :test #'string=) t))

(declaim (inline same-word))
(defn same-word ((a string) (b string)) boolean
  "Whether A and B are the same word. The length check rules out most pairs
before comparing characters."
  (and (= (length a) (length b)) (string= a b)))

(defn match-at ((parts list) (words simple-vector) (wi fixnum) (parser function))
    (values (or null fixnum) slots)
  "PARTS matched at WORDS from WI: the end position and the slots, or nil.
PARSER gives a slot's parser by name."
  (if (null parts)
      (values wi nil)
      (destructuring-bind (kind a &optional optional) (first parts)
        (ecase kind
          (:word (match-alts a (rest parts) words wi parser))
          (:optional
           (multiple-value-bind (end slots) (match-alts a (rest parts) words wi parser)
             (if end
                 (values end slots)
                 (match-at (rest parts) words wi parser))))
          (:slot (match-slot a optional (rest parts) words wi parser))))))

(defn match-alts ((alts list) (rest list) (words simple-vector) (wi fixnum) (parser function))
    (values (or null fixnum) slots)
  "One of ALTS at WORDS from WI, then REST."
  (let ((wi (skip-commas words wi)))
    (dolist (alt alts (values nil nil))
      (when (and (<= (+ wi (length alt)) (length words))
                 (loop for w in alt for i from wi always (same-word w (svref words i))))
        (multiple-value-bind (end slots) (match-at rest words (+ wi (length alt)) parser)
          (when end (return (values end slots))))))))

(defparameter *comma-slots* '("text" "items")
  "Slots whose parsers see commas, which the others never do.")

(defn match-slot ((name string) (optional boolean) (rest list) (words simple-vector) (wi fixnum)
                  (parser function))
    (values (or null fixnum) slots)
  "Slot NAME at WORDS from WI, longest first so \"{duration} timer\" still
finds \"timer\", then REST."
  (let ((parse (funcall parser name))
        (wi (skip-commas words wi))
        (commas (member name *comma-slots* :test #'string=)))
    (unless parse (return-from match-slot (values nil nil)))
    (loop for end from (length words) above wi
          unless (comma-p (svref words (1- end)))
          do (let* ((part (coerce (subseq words wi end) 'list))
                    (next-at (skip-commas words end))
                    (next (and (< next-at (length words)) (svref words next-at)))
                    (value (funcall parse (if commas part (remove-if #'comma-p part)) next)))
               (when value
                 (multiple-value-bind (stop slots) (match-at rest words end parser)
                   (when stop (return-from match-slot (values stop (acons name value slots))))))))
    (if optional
        (match-at rest words wi parser)
        (values nil nil))))

(defn ignored ((words list)) fixnum
  (count-if-not (lambda (w) (member w *fillers* :test #'string=)) words))

(defvar *present* (cons nil nil)
  "The last word list asked about and its cue words, as best-match tries
every template on the same words.")

(defn present-cues ((words list)) list
  "The cue words among WORDS. No other word can start a template's part."
  (let ((last *present*))
    (if (eq (car last) words)
        (cdr last)
        (let ((cues (loop for w in words for c = (gethash w *cue-words*) when c collect c)))
          (setf *present* (cons words cues))
          cues))))

(defn cued-p ((template template) (words list)) boolean
  "Whether WORDS have a first word of every required part of TEMPLATE."
  (let ((present (present-cues words)))
    (every (lambda (firsts) (some (lambda (w) (member w present :test #'eq)) firsts))
           (template-cues template))))

(defn find-template ((template template) (words list) (parser function))
    (values (or null fixnum) slots)
  "The best match in WORDS: how many non-filler words it ignores, and the
slots. Nil when it doesn't match."
  (let ((v (coerce words 'simple-vector)) best best-slots)
    (unless (cued-p template words)
      (return-from find-template (values nil nil)))
    (dotimes (start (length v))
      (multiple-value-bind (end slots) (match-at (template-parts template) v start parser)
        (when end
          (let ((skipped (+ (ignored (subseq words 0 start)) (ignored (nthcdr end words)))))
            (when (and (or (not (template-anchored template)) (zerop skipped))
                       (or (null best) (< skipped best)))
              (setf best skipped best-slots slots))))))
    (values best best-slots)))

(defn spans ((n fixnum) (from fixnum)) list
  "(start . end) of every run of words from FROM to N, longest first."
  (loop for len from (- n from) downto 1
        append (loop for a from from to (- n len) collect (cons a (+ a len)))))

(defn find-slots ((wanted list) (words simple-vector) (from fixnum) (parser function))
    (values boolean slots)
  "Each of WANTED ((name . optional)...) somewhere in WORDS from FROM, in order,
longest span first."
  (when (null wanted) (return-from find-slots (values t nil)))
  (destructuring-bind (name . optional) (first wanted)
    (let ((parse (funcall parser name)) (n (length words)))
      (loop for (a . b) in (and parse (spans n from))
            for part = (coerce (subseq words a b) 'list)
            for value = (and (not (member +comma+ part :test #'equal))
                             (funcall parse part (and (< b n) (svref words b))))
            when value
            do (multiple-value-bind (ok slots) (find-slots (rest wanted) words b parser)
                 (when ok (return-from find-slots (values t (acons name value slots))))))
      (if (and parse optional)
          (find-slots (rest wanted) words from parser)
          (values nil nil)))))

(defn slots-anywhere ((template template) (words list) (parser function)) (values boolean slots)
  "The slot values found anywhere in WORDS, ignoring the template's words.
Nil when a required one is missing or a slot is open."
  (if (has-open-slots template)
      (values nil nil)
      (find-slots (slot-names template) (coerce words 'simple-vector) 0 parser)))

(defn part-choices ((part list)) list
  "A part's possible words, shortest first, an optional one starting empty."
  (destructuring-bind (kind a &optional optional) part
    (declare (ignore optional))
    (ecase kind
      (:word (sort (copy-list a) #'< :key #'length))
      (:optional (cons nil a)))))

(defn variants ((template template)) list
  "The plainest sentence (shortest alternatives, nothing optional) and each
variant of it with one or two parts changed: how commands are usually said
(\"tell a joke\", \"tell me a joke\"). Empty with slots."
  (when (has-slots template) (return-from variants nil))
  (let* ((choices (coerce (mapcar #'part-choices (template-parts template)) 'simple-vector))
         (n (length choices))
         (pick (make-array n :initial-element 0))
         (out nil))
    (flet ((say ()
             (unwords (loop for c across choices for i across pick append (nth i c)))))
      (push (say) out)
      (dotimes (i n)
        (loop for a from 1 below (length (svref choices i))
              do (setf (aref pick i) a)
              (push (say) out)
              (loop for j from (1+ i) below n
                    do (loop for b from 1 below (length (svref choices j))
                             do (setf (aref pick j) b)
                             (push (say) out))
                    (setf (aref pick j) 0)))
        (setf (aref pick i) 0)))
    (nreverse out)))

(defn template-sentences ((template template) (values function)) list
  "The plainest sentences TEMPLATE accepts when its one required slot takes
each of `(funcall VALUES slot)`: shortest alternatives, nothing optional.
Empty when a slot has no fixed set of values."
  (let (prefix suffix (options :none))
    (dolist (part (template-parts template))
      (destructuring-bind (kind a &optional optional) part
        (flet ((add (ws) (if (eq options :none)
                             (setf prefix (append prefix ws))
                             (setf suffix (append suffix ws)))))
          (ecase kind
            (:word (add (reduce (lambda (x y) (if (< (length y) (length x)) y x)) a)))
            (:optional)
            (:slot (unless optional
                     (unless (eq options :none) (return-from template-sentences nil))
                     (setf options (or (funcall values a)
                                       (return-from template-sentences nil)))))))))
    (unless (eq options :none)
      (mapcar (lambda (v) (unwords (append prefix v suffix))) options))))

(defn expand ((template template) (values function) (cap (integer 0))) list
  "Up to CAP sentences spread over every combination of alternatives, optional
words and slot VALUES: examples for the intent classifier."
  (let* ((choices
          (loop for (kind a optional) in (template-parts template)
                collect (coerce
                         (ecase kind
                           (:word (mapcar #'unwords a))
                           (:optional (cons "" (mapcar #'unwords a)))
                           (:slot (let ((v (funcall values a)))
                                    (unless v (return-from expand nil))
                                    (if optional (cons "" v) v))))
                         'simple-vector)))
         (total (reduce #'* choices :key #'length))
         (n (min total cap)))
    (loop for k below n
          collect (let ((i (if (= n total) k (* k (floor total n)))))
                    (unwords (loop for c in choices
                                   for w = (svref c (mod i (length c)))
                                   do (setf i (floor i (length c)))
                                   unless (string= w "") collect w))))))
