;;; Questions about the inbox.

(in-package #:va)

(in-skill info :continued t)

(defparameter *inbox* "folder:thalheim.io"
  "The notmuch query for mail sent to me, rather than lists and spam.")

(defreplies *no-mail*
  "No unread mail. Inbox zero, you legend."
  "Nothing new. Nobody loves you. Except me.")

(defcommand info-mail
  "(do|have) i (have|got) [any] [new|unread] (mail|email|emails|mails|messages)"
  "(any|how many) [new|unread] (mail|email|emails|mails)"
  "check [my] (mail|email|emails|inbox)"
  :examples ("Do I have new mail?" "Check my email")
  (let* ((out (output "notmuch" "count" (format nil "~a and tag:unread and date:7d.." *inbox*)))
         (n (or (and out (parse-integer out :junk-allowed t)) 0)))
    (live "Mail" (format nil "~d unread" n)
          (cond ((= n 0) (pick *no-mail*))
                ((= n 1) "One unread email. Just one. Go on, read it.")
                ((> n 50) (format nil "~d unread emails. Declaring email bankruptcy?" n))
                (t (format nil "You have ~d unread emails." n))))))

(defn mail-author ((authors string)) string
  "The first of notmuch's authors, \"a@b.c\" said as \"a at b.c\"."
  (let ((first (trim (subseq authors 0 (position-if (lambda (c) (find c ",|")) authors)))))
    (cl-ppcre:regex-replace-all "@" first " at ")))

(defn mail-subject ((subject string)) string
  "SUBJECT without replies' prefixes, list tags and emoji, at most 12 words."
  (let* ((s (cl-ppcre:regex-replace-all "(?i)^(\\s*(re|aw|antw):\\s*)+" subject ""))
         (s (cl-ppcre:regex-replace-all "\\[[^]]*\\]\\s*" s ""))
         (s (remove-if (lambda (c) (and (> (char-code c) #x2000) (not (alpha-char-p c)))) s))
         (s (trim (cl-ppcre:regex-replace-all "\\s+" s " ")))
         (ws (split-words s)))
    (if (> (length ws) 12) (format nil "~a..." (unwords (subseq ws 0 12))) s)))

(defn read-threads ((raw string)) list
  "(author subject) per thread of `notmuch search --format=json'."
  (let ((threads (parse-json raw)))
    (and (vectorp threads)
         (loop for th across threads
               for authors = (json-get th "authors")
               for subject = (json-get th "subject")
               when (and (stringp authors) (stringp subject))
               collect (list (mail-author authors) (mail-subject subject))))))

(defn inbox-threads ((query string) (most (integer 1))) list
  (read-threads (or (output "notmuch" "search" "--format=json" "--sort=newest-first"
                            (format nil "--limit=~d" most)
                            (format nil "~a and (~a)" *inbox* query))
                    "")))

(defn said-threads ((threads list)) string
  (format nil "~{~a~^ ~}" (loop for (from subject) in threads
                                collect (format nil "From ~a: ~a." from subject))))

(defreplies *mail-read*
  "~a" "Fresh from the inbox. ~a" "~a That's all the gossip.")

(defcommand info-mail-read
  "!(read|tell) [me] [my] [new|latest|last|recent|unread] (mail|mails|email|emails|messages)"
  "!(what are|what's|what is) [in] my (latest|last|new|recent|unread) (mail|mails|email|emails)"
  "!(what's|what is) in my inbox"
  (:join "!who (emailed|mailed|wrote|has written|has emailed|sent) [me] [an email|mail]"
         "[today|lately|recently]")
  :kind "MailRead"
  :examples ("Read my emails" "Who emailed me?")
  (let ((threads (inbox-threads "tag:unread and date:7d.." 3)))
    (if threads
        (live "Mail" (format nil "~d new" (length threads))
              (pick *mail-read* (said-threads threads)))
        (live "Mail" "" (pick *no-mail*)))))

(defreplies *no-mail-from*
  "Nothing from ~a lately. Ghosted?"
  "No mail from ~a. Maybe write first?")

(defcommand info-mail-from
  "!(any|is there|is there any) [new] (mail|email|emails|message) from {person}"
  "!(do|did) i (have|get|got) [any] [new] (mail|email|emails) from {person}"
  "!(did|has) {person} (email|emailed|mail|mailed|write|written|wrote) [to] me"
  "!(when|what) did {person} last (email|write) [me]"
  :kind "MailFrom"
  :slots ((name coalton:String))
  :parse (let ((p (slot "person"))) (and p (list p)))
  :examples ("Any mail from Shannan?" "Did Bob email me?")
  (let ((threads (inbox-threads (format nil "from:\"~a\"" name) 1)))
    (if threads
        (live "Mail" name (said-threads threads))
        (live "Mail" "" (pick *no-mail-from* (string-capitalize name))))))

(defskill info)
