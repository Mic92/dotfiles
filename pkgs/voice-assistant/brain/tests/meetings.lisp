;;; Ported from meetings.rs.

(in-package #:va-tests)

(defmacro at-ten (&body body)
  "BODY at 10:00 on Wednesday 2025-10-22."
  `(let ((va::*context* (va::make-context (* 10 3600) (va::day-number 2025 10 22))))
     ,@body))

(define-test meetings-phrasings
  :parent va-tests
  (at-ten
   (parses-as "~a"
              '(("When is my next meeting?" "Meeting(Next)")
                ("What's next on my calendar?" "Meeting(Next)")
                ("How long until my next meeting?" "Meeting(Next)")
                ("Join my meeting." "Meeting(Join)")
                ("Open the zoom link" "Meeting(Join)")
                ("Start the call" "Meeting(Join)")
                ("What's on my calendar today?" "Meeting(Day { day: 0, part: None })")
                ("Do I have any meetings today?" "Meeting(Day { day: 0, part: None })")
                ("What do I have tomorrow?" "Meeting(Day { day: 1, part: None })")
                ("How busy am I tomorrow?" "Meeting(Day { day: 1, part: None })")
                ("Agenda" "Meeting(Day { day: 0, part: None })")
                ("Am I free this afternoon?" "Meeting(Day { day: 0, part: Some(5) })")
                ("What do I have tomorrow morning?" "Meeting(Day { day: 1, part: Some(3) })")
                ("Am I free at 3?" "Meeting(FreeAt { day: 0, secs: 54000 })")
                ("Am I free tomorrow at 10 am?" "Meeting(FreeAt { day: 1, secs: 36000 })")
                ("Do I have a meeting at 4 pm?" "Meeting(FreeAt { day: 0, secs: 57600 })")
                ("What's on my calendar on friday?" "Meeting(Day { day: 2, part: None })")
                ("what do i have going on may 4th" "Meeting(Day { day: 194, part: None })")
                ("what do i have on my calendar for march 2"
                 "Meeting(Day { day: 131, part: None })")
                ("check my calendar for saturday" "Meeting(Day { day: 3, part: None })")
                ("read my appointments for march 2" "Meeting(Day { day: 131, part: None })")
                ("what's my schedule like for thursday the 14th"
                 "Meeting(Day { day: 23, part: None })")
                ("is anything happening on march 4th 2019" "Meeting(Day { day: 133, part: None })")
                ("anything on the schedule for october 24th" "Meeting(Day { day: 2, part: None })")
                ("do i have anything on my calendar next sunday"
                 "Meeting(Day { day: 4, part: None })")
                ("is my calendar free next sunday" "Meeting(Day { day: 4, part: None })")
                ("what do i have scheduled on march 2nd on my calendar"
                 "Meeting(Day { day: 131, part: None })")
                ("how is my calendar looking for may 6th" "Meeting(Day { day: 196, part: None })")
                ("what's happening on may 3rd" "Meeting(Day { day: 193, part: None })")
                ("tell me what is showing on my calendar for the 17th of march"
                 "Meeting(Day { day: 146, part: None })")
                ("what do i have to do on january 1st" "Meeting(Day { day: 71, part: None })")
                ("add dentist appointment to my calendar for march 1"
                 "Meeting(Ask { title: \"dentist appointment\", day: Some(130) })")
                ("i need to add farm trip to my calendar for friday"
                 "Meeting(Ask { title: \"farm trip\", day: Some(2) })")
                ("add my dentist appointment to the calendar"
                 "Meeting(Ask { title: \"dentist appointment\", day: None })")
                ("please add final exams to my calendar for may 2"
                 "Meeting(Ask { title: \"final exams\", day: Some(192) })")
                ("Am I free on march 1st at 10 am?" "Meeting(FreeAt { day: 130, secs: 36000 })")))
   (parses-as "Meeting(Create { title: ~s, day: ~d, secs: ~d, confirmed: false })"
              '(("Schedule a meeting with Anna on friday at 3 pm." "Meeting with anna" 2 54000)
                ("Schedule a meeting with Anna tomorrow at 3 pm." "Meeting with anna" 1 54000)
                ("Add dentist to my calendar tomorrow at 9 am" "dentist" 1 32400)
                ("Book a call about the budget at 2" "the budget" 0 50400)))))

(defun dentist (day secs)
  (format nil "Meeting(Create { title: \"dentist\", day: ~d, secs: ~d, confirmed: false })"
          day secs))

(define-test meetings-missing-times-are-asked-for
  :parent va-tests
  (at-ten
   (is equal (dentist 2 (* 15 3600)) (answered (va::ask-meeting "dentist" 2) "at 3"))
   (is equal (dentist 2 (* 9 3600)) (answered (va::ask-meeting "dentist" nil) "friday at 9 am"))
   (is equal (dentist 1 (* 10 3600)) (answered (va::ask-meeting "dentist" nil) "tomorrow at 10"))
   (is equal "Dismiss" (answered (va::ask-meeting "dentist" nil) "never mind"))))

(define-test meetings-changes-are-not-read-out
  :parent va-tests
  (at-ten
   (parses-not "Meeting(Day" '("please take final exams off my calendar for may 3"
                               "you can remove the wedding from my calendar for march 12, 2019"
                               "get rid of everything on my calendar for march 2nd"
                               "i need a clear calendar for april 15"))))

(define-test meetings-days-are-named
  :parent va-tests
  (let ((va::*context* (va::make-context 0 20383)))
    (is equal "today" (va::day-name 0))
    (is equal "on Friday" (va::day-name 2))
    (is equal "on Sunday, March 1" (va::day-name 130))))

(defparameter *khal*
  (format nil "~{~a~^~%~}"
          (list (concatenate 'string "[{\"start-long\": \"2026-10-23 14:00\", \"end-long\": \"2"
                             "026-10-23 14:40\", \"title\": \"Anthropic sync\", \"loca"
                             "tion\": \"https://meet.google.com/xqm-fcdh-vmi\", \"ur"
                             "l\": \"\", \"description\": \"\", \"all-day\": \"False\"}]")
                "[]"
                (concatenate 'string "[{\"start-long\": \"2026-10-24\", \"end-long\": \"2026-10"
                             "-25\", \"title\": \"Holiday\", \"location\": \"\", \"url\": \""
                             "\", \"description\": \"\", \"all-day\": \"True\"}, {\"start-"
                             "long\": \"2026-10-24 09:00\", \"end-long\": \"2026-10-24"
                             " 09:30\", \"title\": \"Standup\", \"location\": \"Room 4\","
                             " \"url\": \"\", \"description\": \"Join: <https://zoom.us"
                             "/j/123>\", \"all-day\": \"False\"}]")))
  "khal list output for three days, the middle one empty.")

(define-test meetings-events
  :parent va-tests
  (let ((ev (va::parse-events *khal*)))
    (is = 3 (length ev))
    (is equal "https://meet.google.com/xqm-fcdh-vmi" (va::event-link (first ev)))
    (true (va::event-all-day (second ev)))
    (is equal "https://zoom.us/j/123" (va::event-link (third ev)))
    (flet ((next (now) (multiple-value-bind (e mins) (va::current-or-next ev now)
                         (list (and e (va::event-title e)) mins))))
      (is equal '("Anthropic sync" 25) (next "2026-10-23 13:35"))
      (is equal '("Anthropic sync" -10) (next "2026-10-23 14:10"))
      (is equal "Standup" (first (next "2026-10-23 15:00")))
      (false (first (next "2026-10-25 00:00"))))))

(define-test meetings-time-maths
  :parent va-tests
  (is = 1440 (- (va::minutes "2026-03-01 00:00") (va::minutes "2026-02-28 00:00")))
  (is = 2880 (- (va::minutes "2024-03-01") (va::minutes "2024-02-28")))
  (is = 90 (- (va::minutes "1970-01-02 01:30") (va::minutes "1970-01-02")))
  (is equal "25 minutes" (va::spoken-mins 25))
  (is equal "an hour and 15 minutes" (va::spoken-mins 75))
  (is equal "2 PM" (va::event-hm "2026-10-23 14:00"))
  (is equal "2:30 PM" (va::event-hm "2026-10-23 14:30")))

(define-test meetings-next-names-the-day
  :parent va-tests
  (let ((va::*context* (va::make-context 0 (va::day-number 2026 10 22))))
    (is equal "tomorrow" (va::event-day-name (first (va::parse-events *khal*))))))
