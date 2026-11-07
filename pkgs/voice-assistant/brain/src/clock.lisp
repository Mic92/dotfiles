(in-package #:va)

;;; Spoken clock times: "7:30 pm", "half past four", "noon", "seven oh five".

(alexandria:define-constant +day+ 86400)

(defn seconds-from ((clock va.types:Clock) (now (integer 0))) (integer 1)
  "Seconds until CLOCK's next occurrence. Equal to now means tomorrow."
  (flet ((until (time)
           (let ((d (mod (- (+ time +day+) (mod now +day+)) +day+)))
             (if (zerop d) +day+ d))))
    (let ((secs (va.types:clock-secs clock)))
      (if (va.types:clock-twelve-hour clock)
          (min (until secs) (until (mod (+ secs (/ +day+ 2)) +day+)))
          (until secs)))))

(defn seconds-on ((clock va.types:Clock) (now (integer 0)) (day (integer 0)))
    (or null (integer 0))
  "Seconds until CLOCK DAY days from today. Without am/pm the earlier reading
wins for future days (\"tomorrow at 7\" is morning) and the next one still
ahead for today. Nil if it already passed."
  (let ((now (mod now +day+))
        (secs (va.types:clock-secs clock)))
    (if (plusp day)
        (- (+ (* day +day+) secs) now)
        (let ((ahead (find-if (lambda (time) (> time now))
                              (if (va.types:clock-twelve-hour clock)
                                  (list secs (+ secs (/ +day+ 2)))
                                  (list secs)))))
          (and ahead (- ahead now))))))

(defn without-suffix ((ws words) (suffix list)) (or null words)
  "WS without SUFFIX at its end, or nil when it doesn't end so."
  (let ((n (- (length ws) (length suffix))))
    (and (>= n 0) (equal (nthcdr n ws) suffix) (subseq ws 0 n))))

(defparameter *qualifiers*
  '((("am") :am) (("pm") :pm)
    (("in" "the" "morning") :am) (("in" "the" "afternoon") :pm) (("in" "the" "evening") :pm)
    (("at" "night") :pm) (("tonight") :pm) (("o'clock") nil))
  "Endings that make words a time of day, and the half of the day they name.")

(defn qualifier ((ws words)) (values words (member nil :am :pm) boolean)
  "WS without a trailing am/pm or o'clock, the half of the day, and whether
there was one."
  (loop for (suffix half) in *qualifiers*
        for rest = (without-suffix ws suffix)
        when rest do (return-from qualifier (values rest half t)))
  (values ws nil nil))

(defn below ((n (or null integer)) (limit integer)) (or null integer)
  "N when it is under LIMIT."
  (and n (< n limit) n))

(defn clock-minutes ((ws words)) (or null (integer 0 59))
  (trivia:match ws
                (nil 0)
                ((list (or "oh" "o") m) (below (number m) 10))
                ((list tens ones) (below (tens-ones tens ones) 60))
                ((list m) (below (number m) 60))))

(defn hour-minute ((ws words)) (values (or null (integer 0 23)) (or null (integer 0 59)))
  "\"7\", \"7:30\", \"seven thirty\", \"seven oh five\", \"19 45\"."
  (multiple-value-bind (h m)
      (trivia:match ws
                    ((list (trivia:guard hm (find #\: hm)))
                     (let ((at (position #\: hm)))
                       (values (digits (subseq hm 0 at))
                               (below (digits (subseq hm (1+ at))) 60))))
                    ((list* h rest) (values (number h) (clock-minutes rest))))
    (if (and h m (< h 24)) (values h m) (values nil nil))))

(defn clock-offset ((ws words)) (or null (integer 1 59))
  "Minutes before \"past\" or \"to\": \"half\", \"a quarter\", \"20 minutes\"."
  (let* ((ws (if (equal (first ws) "a") (rest ws) ws))
         (ws (or (without-suffix ws '("minutes")) (without-suffix ws '("minute")) ws)))
    (trivia:match ws
                  ((list "half") 30)
                  ((list "quarter") 15)
                  ((list n) (let ((n (number n))) (and n (< 0 n 60) n)))
                  ((list tens ones) (below (tens-ones tens ones) 60)))))

(defn relative-time ((ws words)) (values (or null (integer 0 23)) (or null (integer 0 59)))
  "\"half past four\", \"quarter to five\", \"ten past nine\", \"20 minutes to 6\"."
  (let* ((idx (position-if (lambda (w) (in w "past" "after" "to" "before")) ws))
         (offset (and idx (clock-offset (subseq ws 0 idx))))
         (hour (and idx (trivia:match (nthcdr (1+ idx) ws) ((list h) (below (number h) 24))))))
    (cond ((not (and offset hour)) (values nil nil))
          ((in (nth idx ws) "past" "after") (values hour offset))
          ;; "half to" is not English.
          ((= offset 30) (values nil nil))
          (t (values (mod (+ hour 23) 24) (- 60 offset))))))

(defn parse-clock ((ws words)) (or null va.types:Clock)
  (trivia:match ws
                ((list (or "noon" "midday")) (va.types:Clock (* 12 3600) nil t))
                ((list "midnight") (va.types:Clock 0 nil t))
                (_ (spoken-clock ws))))

(defn spoken-clock ((ws words)) (or null va.types:Clock)
  "\"half past seven\", \"7:30 pm\", \"ten in the evening\"."
  (multiple-value-bind (core half qualified) (qualifier ws)
    (multiple-value-bind (rh rm) (relative-time core)
      (multiple-value-bind (h m) (if rh (values rh rm) (hour-minute core))
        (when (and h (not (and half (or (zerop h) (> h 12)))))
          (let ((h (cond ((and (eq half :pm) (< h 12)) (+ h 12))
                         ((and (eq half :am) (= h 12)) 0)
                         (t h))))
            (va.types:Clock (+ (* h 3600) (* m 60))
                            (and (null half) (<= 1 h 12))
                            (or qualified (and rh t)))))))))

(defn format-hm ((secs (integer 0))) string
  "\"7:30 PM\", \"12 PM\"."
  (multiple-value-bind (h rest) (floor secs 3600)
    (let ((m (floor rest 60))
          (h12 (if (zerop (mod h 12)) 12 (mod h 12)))
          (suffix (if (< h 12) "AM" "PM")))
      (if (zerop m)
          (format nil "~d ~a" h12 suffix)
          (format nil "~d:~2,'0d ~a" h12 m suffix)))))
