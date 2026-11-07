;;; Headlines from RSS and Atom feeds.

(in-package #:va)

(in-skill news)

(defreplies *intro*
  "Here's what's going on. Brace yourself."
  "The news, in brief, before it gets worse."
  "Headlines. Read with one eye closed."
  "The news. Sit down for this one.")

(defreplies *no-news*
  "No news. Either the world is fine or the internet is down."
  "The news won't load. Blissful ignorance it is.")

(defreplies *news-filler*
  "Fetching the headlines."
  "One sec, reading the papers."
  "Let me see who's ruining the world today.")

(defparameter *entities*
  '(("&amp;" . "&") ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'") ("&lt;" . "<")
    ("&gt;" . ">")))

(defn unescape ((s string)) string
  (let ((s (string-trim '(#\Space #\Newline #\Tab) s)))
    (when (and (uiop:string-prefix-p "<![CDATA[" s) (uiop:string-suffix-p s "]]>"))
      (setf s (subseq s 9 (- (length s) 3))))
    (loop for (from . to) in *entities*
          do (setf s (uiop:frob-substrings s (list from) to)))
    (string-trim '(#\Space #\Newline #\Tab) s)))

(defn headlines ((xml string)) list
  "Titles of <item> (RSS) or <entry> (Atom) elements, skipping the channel's
title, livestreams and videos."
  (let (out)
    (loop for (open close) in '(("<item" "</item>") ("<entry" "</entry>"))
          do (loop with start = 0
                   for i = (search open xml :start2 start)
                   for end = (and i (search close xml :start2 i))
                   while end
                   do (let* ((tag (search "<title" xml :start2 i :end2 end))
                             (gt (and tag (position #\> xml :start tag :end end)))
                             (stop (and gt (search "</title>" xml :start2 gt :end2 end)))
                             (title (and stop (unescape (subseq xml (1+ gt) stop)))))
                        (when (and title (plusp (length title))
                                   (not (uiop:string-prefix-p "Livestream" title))
                                   (not (uiop:string-prefix-p "Watch:" title)))
                          (push title out)))
                   (setf start end)))
    (nreverse out)))

(defn news ((url string) (name string)) reply
  (let* ((raw (http-get url))
         (items (and raw (headlines raw))))
    (if (null items)
        (reply "News" "" (pick *no-news*) :live t)
        (reply name
               (format nil "~{• ~a~^~%~}" (subseq items 0 (min 8 (length items))))
               (format nil "~a ~{~a~^. Next: ~}" (pick *intro*)
                       (subseq items 0 (min 3 (length items))))
               :live t))))

(defcommand news-world
  "!(what's|what is) (the|in the) news [today]"
  "!(what's|what is) happening in the world"
  "!(read|tell|give) me the (news|headlines)"
  "![the|today's] headlines [please]"
  "!any news"
  "!(news|headlines) (update|briefing)"
  :filler *news-filler*
  :repeatable t
  :examples ("What's the news?" "Read me the headlines")
  (news "https://feeds.bbci.co.uk/news/rss.xml" "BBC News"))

(defcommand news-tech
  "!(what's|what is) (on|new on) hacker news"
  "![read|tell|give] [me] [the] (tech|nerd|computer) (news|headlines)"
  "!hacker news [headlines|please]"
  "!(what's|what is) new in tech"
  :filler *news-filler*
  :repeatable t
  :examples ("What's on Hacker News?" "Tech news")
  (news "https://hnrss.org/frontpage?points=200" "Hacker News"))

(defskill news)
