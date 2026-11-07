;;; Web questions answered by Kagi's Quick Answer (`kagi-search -j`).

(in-package #:va)

(in-skill kagi)

(defreplies *thinking-fail*
  "The internet had nothing on that. Rare, but it happens."
  "Kagi shrugged. I'm shrugging too, in spirit."
  "Even the internet gave up on that one. Impressive.")

(defreplies *kagi-filler*
  "Let me ask the internet."
  "Looking it up."
  "Hang on, consulting the hive mind."
  "Googling. Don't tell Kagi I said that.")

(defparameter *question-words*
  '("who" "who's" "whom" "whose" "what" "what's" "when" "where" "where's" "why" "how" "how's"
    "which"))

;; A whole question: a question word and at least two more words, so "what?"
;; or "how are you" style fragments stay with chat.
(defslot "question" (ws next)
  (and (member (first ws) *question-words* :test #'string=) (cddr ws)
       (format nil "~{~a~^ ~}" ws)))

(defn markdown-inline ((line string)) string
  "LINE without emphasis, code marks, links and footnote references.
\"metres [^2].\" must not become \"metres .\""
  (let ((out (make-string-output-stream)) (i 0) (n (length line)))
    (loop while (< i n)
          do (let ((c (char line i)))
               (cond ((and (char= c #\[) (< (1+ i) n) (char= (char line (1+ i)) #\^))
                      (let ((close (position #\] line :start i)))
                        (cond (close
                               (let ((sofar (get-output-stream-string out)))
                                 (write-string (string-right-trim " " sofar) out)
                                 (setf i (1+ close))))
                              (t (write-char c out) (incf i)))))
                     ((find c "*_`") (incf i))
                     (t (write-char c out) (incf i)))))
    (get-output-stream-string out)))

(defn sentences ((text string)) list
  "TEXT split after . ! or ? when a capital, digit or quote follows, as
Unicode sentence bounds do: \"e.g. in memes\" stays whole."
  (let ((ws (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline))
                    :test #'string=))
        (out nil) (cur nil))
    (loop for (w next) on ws
          do (push w cur)
          (when (or (null next)
                    (and (find (char w (1- (length w))) ".!?")
                         (let ((c (char next 0)))
                           (or (upper-case-p c) (digit-char-p c) (find c "\"'")))))
            (push (format nil "~{~a~^ ~}" (reverse cur)) out)
            (setf cur nil)))
    (nreverse out)))

(defn markdown-blocks ((md string)) list
  "Kagi's markdown as (item-p . sentences) for each paragraph or list item,
without headings and footnote definitions."
  (let (blocks lines item)
    (flet ((flush ()
             (let ((s (sentences (format nil "~{~a~^ ~}" (reverse lines)))))
               (when s (push (cons item s) blocks)))
             (setf lines nil item nil)))
      (dolist (raw (uiop:split-string md :separator '(#\Newline)))
        (let ((line (string-trim " " raw)))
          (cond ((string= line "") (flush))
                ((or (uiop:string-prefix-p "#" line) (uiop:string-prefix-p "[^" line)) (flush))
                ((or (uiop:string-prefix-p "- " line) (uiop:string-prefix-p "* " line))
                 (flush)
                 (setf item t)
                 (push (markdown-inline (subseq line 2)) lines))
                (t (push (markdown-inline line) lines)))))
      (flush))
    (nreverse blocks)))

(defn spoken ((md string) (most (integer 0))) string
  "The first MOST sentences. A lead-in ending in a colon is followed by the
first sentence of each list item after it, five at most."
  (let ((said nil) (blocks (markdown-blocks md)))
    (loop while blocks
          do (dolist (s (cdr (pop blocks)))
               (when (>= (length said) most)
                 (return-from spoken (format nil "~{~a~^ ~}" (reverse said))))
               (push s said))
          (when (and said (uiop:string-suffix-p (first said) ":"))
            (loop for b in blocks while (car b) for i from 0
                  when (< i 5) do (push (second b) said))
            (return)))
    (format nil "~{~a~^ ~}" (reverse said))))

(defn parse-answer ((raw string)) (values (or null string) (or null string))
  "The quick answer's markdown and its first reference."
  (let* ((v (handler-case (jzon:parse raw) (error () nil)))
         (md (json-get v "quick_answer" "markdown"))
         (url (json-get v "quick_answer" "references" 0 "url")))
    (if (and (stringp md) (plusp (length (string-trim " " md))))
        (values md (and (stringp url) url))
        (values nil nil))))

(defn ask-kagi ((q string)) reply
  (let ((raw (output "kagi-search" "-j" q)))
    (multiple-value-bind (md url) (if raw (parse-answer raw) (values nil nil))
      (if (null md)
          (reply "Kagi" q (pick *thinking-fail*) :live t)
          (reply (format nil "Kagi: ~a" q)
                 (format nil "~a~@[~%~a~]" (spoken md 6) url)
                 (spoken md 3) :live t)))))

(defmacro defquery (name &body templates-and-keys)
  `(defcommand ,name ,@templates-and-keys
               :kind "Query"
               :slots ((query coalton:String))
               :filler *kagi-filler*
               (ask-kagi query)))

(defquery kagi-search
    "!(search|google|kagi|look up) [the web|the internet|online] [for] {text}"
  "!(ask|use) kagi [about] {text}"
  "!(can you|could you|please) (search|google|look up) [for] {text}"
  :parse (list (slot "text"))
  :examples ("Search for the best pizza in Munich" "Ask Kagi about black holes"))

(defquery kagi-find-out
    "!(find out|tell me) {question}"
  :parse (list (slot "question"))
  :examples ("find out why the sky is blue"))

;; Open questions, asked whole, when nothing else understood them.
(defquery kagi-question
    "!{question}"
  :parse (list (slot "question"))
  :fallback t
  :examples ("Who was Ada Lovelace?"))

(defskill kagi)
