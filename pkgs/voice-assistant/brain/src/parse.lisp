;;; From words to an action: unwrapping the request, then the best match.

(in-package #:va)

(defparameter *lead-ins*
  '(("can" "you" "please" "tell" "me") ("can" "you" "tell" "me") ("could" "you" "tell" "me")
    ("would" "you" "tell" "me") ("would" "you" "let" "me" "know") ("please" "tell" "me")
    ("tell" "me") ("i" "need" "to" "know") ("i" "want" "to" "know") ("i" "wanna" "know")
    ("i'd" "like" "to" "know") ("i" "would" "like" "to" "know") ("i" "wonder")
    ("do" "you" "know") ("let" "me" "know") ("inform" "me") ("look" "up") ("please" "find")
    ("find" "out"))
  "Ways of asking that wrap the actual question.")

(defparameter *tails*
  '(("for" "me") ("please") (",") ("," "i" "need" "to" "know") ("," "can" "you" "tell" "me"))
  "Endings that wrap the actual question, as in \", can you tell me\".")

(defn unwrap-request ((ws words)) words
  "\"can you tell me what time it is, please\" -> \"what time it is\"."
  (loop
   (let ((before (length ws)))
     (loop while (in (first ws) "," "please" "alexa") do (pop ws))
     (let ((lead (find-if (lambda (l) (a:starts-with-subseq l ws :test #'equal)) *lead-ins*)))
       (when lead (setf ws (nthcdr (length lead) ws))))
     (let ((tail (find-if (lambda (s) (a:ends-with-subseq s ws :test #'equal)) *tails*)))
       (when tail (setf ws (butlast ws (length tail)))))
     (when (= (length ws) before) (return ws)))))

(defn resolve-dates ((slots slots)) (values slots boolean)
  "Calendar dates as days from today, the only form skills see. False when
a date doesn't exist."
  (let ((ok t))
    (values (loop for (name . v) in slots
                  collect (cons name
                                (if (typep v 'va.types:Date)
                                    (or (days-ahead v (context-today *context*)) (setf ok nil))
                                    v)))
            ok)))

(defn best-match ((ws words) (fallback boolean)) (or null action)
  "The command whose template matches with the fewest ignored words, ties
going to the earlier one."
  (let (best best-skipped)
    (dolist (c *commands* best)
      (when (eq (command-fallback c) fallback)
        (dolist (tpl (command-parsed c))
          (multiple-value-bind (skipped slots) (find-template tpl ws #'slot-parser)
            (when (and skipped (or (null best) (< skipped best-skipped)))
              (multiple-value-bind (slots ok) (resolve-dates slots)
                (let ((fields (and ok (funcall (command-parse c) slots (template-source tpl)))))
                  (when fields
                    (setf best (make-action c (if (eq fields t) nil fields))
                          best-skipped skipped)))))))))))

(defn parse-tier ((text string) (fallback boolean)) (or null action)
  "Only the catch-alls (FALLBACK) or only the rest. The question inside a
request beats a catch-all on the whole of it."
  (let* ((ws (normalize text))
         (core (unwrap-request ws)))
    (or (and (not fallback) (not (equal core ws)) (best-match core nil))
        (best-match ws fallback)
        (and (not fallback)
             (let ((act (dialogue-act core))) (and act (ambient act)))))))

(defn parse-text ((text string)) (or null action)
  (or (parse-tier text nil) (parse-tier text t)))

(defn perform ((a action)) reply
  "Run A. A plain string from the command is its speech."
  (let ((out (let ((*late* (late-p)))
               (apply (command-run (action-command a)) (action-fields a)))))
    (if (stringp out) (reply (command-kind (action-command a)) "" out) out)))
