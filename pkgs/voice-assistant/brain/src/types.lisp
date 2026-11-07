;;; Slot values and their parts, as Coalton types.

(defpackage #:va.types
  (:use #:coalton #:coalton-prelude)
  (:export #:Clock #:clock-secs #:clock-twelve-hour #:clock-marked
           #:Date #:Weekday #:MonthDay #:OfMonth))

(in-package #:va.types)

(coalton-toplevel
 (define-type Clock
     "Seconds since midnight, whether it may also be 12 hours later (no am/pm
for 1 to 12), and whether it is unmistakably a time of day."
   (Clock UFix Boolean Boolean))

 (declare clock-secs (Clock -> UFix))
 (define (clock-secs (Clock s _ _)) s)
 (declare clock-twelve-hour (Clock -> Boolean))
 (define (clock-twelve-hour (Clock _ h _)) h)
 (declare clock-marked (Clock -> Boolean))
 (define (clock-marked (Clock _ _ m)) m)

 (define-type Date
     "A date as said, resolved against today later."
   ;; 0 = Sunday, and whether this week's is skipped ("next friday").
   (Weekday UFix Boolean)
   ;; 1-based month, day.
   (MonthDay UFix UFix)
   ;; "the 15th": this month, or the next once it passed.
   (OfMonth UFix)))
