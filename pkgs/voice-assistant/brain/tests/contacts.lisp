;;; Contacts and birthdays from khard.

(in-package #:va-tests)

(defparameter *birthdays*
  (format nil "1992.03.03~cShannan Example~%1958.01.13~cHans-Jürgen~%1990.10.22~cBob Builder~%~
               1985.10.30~cBob Marley~%garbage~%" #\Tab #\Tab #\Tab #\Tab))

(define-test contacts-read
  :parent va-tests
  (let ((people (va::read-birthdays *birthdays*)))
    (is = 4 (length people))
    (is equal '("Shannan Example" 3 3 1992) (first people))
    (flet ((named (heard) (let ((p (va::person-named (va::split-words heard) people)))
                            (and p (first p)))))
      (is equal "Shannan Example" (named "shannan"))
      (is equal "Shannan Example" (named "shannon"))
      (is equal "Shannan Example" (named "shannan example"))
      (is equal "Hans-Jürgen" (named "hans-jürgen"))
      (is equal "Bob Marley" (named "bob marley"))
      (false (named "bob") "two Bobs")
      (false (named "angela merkel")))))

(define-test birthdays-ahead
  :parent va-tests
  (let ((today (va::day-number 2025 10 22))
        (people (va::read-birthdays *birthdays*)))
    (flet ((ahead (name) (va::birthday-ahead (find name people :key #'first :test #'string=)
                                             today)))
      (is equal '(0 35) (multiple-value-list (ahead "Bob Builder")))
      (is equal '(8 40) (multiple-value-list (ahead "Bob Marley")))
      (is equal '(132 34) (multiple-value-list (ahead "Shannan Example"))))
    (is equal '("Bob Builder" "Bob Marley")
        (mapcar #'first (va::birthdays-within people today 30)))))

(define-test contact-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Contact(~a)"
               '(("when is shannan's birthday" "Birthday(\"shannan\")")
                 ("when's shannan's birthday" "Birthday(\"shannan\")")
                 ("how many days until shannan's birthday" "Birthday(\"shannan\")")
                 ("when was bob marley born" "Birthday(\"bob marley\")")
                 ("how old is shannan" "Age(\"shannan\")")
                 ("how old will shannan turn" "Age(\"shannan\")")
                 ("whose birthday is next" "Upcoming")
                 ("any birthdays coming up" "Upcoming")
                 ("is it anyone's birthday today" "Upcoming")
                 ("what's shannan's phone number" "Phone(\"shannan\")")
                 ("what is the number of bob marley" "Phone(\"bob marley\")")
                 ("what's shannan's email address" "Email(\"shannan\")")))
    (is equal "Chat(HowOld)" (parsed "how old are you"))))

(define-test contacts-named-by-their-address
  :parent va-tests
  (let ((cards (va::read-cards (format nil "shannan@lekwati.com~ashannan@lekwati.com~ainternet~%~
                                            hanne.karl@gmx.de~aHanne Karl~awork"
                                       #\Tab #\Tab #\Tab #\Tab))))
    (dolist (heard '("shannon" "shanan" "shannan"))
      (is equal "shannan@lekwati.com" (second (va::person-named (va::split-words heard) cards))
          heard))
    (is equal "Shannan" (va::first-name "shannan@lekwati.com"))
    (is equal "Hanne Karl" (first (va::person-named '("hanne") cards)))))
