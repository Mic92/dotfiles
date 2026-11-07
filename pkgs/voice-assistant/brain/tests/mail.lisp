;;; Reading out mail from notmuch.

(in-package #:va-tests)

(defparameter *threads*
  "[{\"authors\": \"Winter Morycz, NixOS Foundation| John Ericson\",
     \"subject\": \"Re: [nixos] Antithesis potentially sponsoring FreeBSD\",
     \"date_relative\": \"Fri. 18:56\"},
    {\"authors\": \"portal@elster.de\",
     \"subject\": \"Mein ELSTER: Bestätigung der Übermittlung\",
     \"date_relative\": \"Yest. 14:32\"}]")

(define-test mail-read
  :parent va-tests
  (is equal '(("Winter Morycz" "Antithesis potentially sponsoring FreeBSD")
              ("portal at elster.de" "Mein ELSTER: Bestätigung der Übermittlung"))
      (va::read-threads *threads*))
  (is equal '() (va::read-threads "[]"))
  (is equal '() (va::read-threads "garbage"))
  (is equal "Campfire @ Mon, Grüße" (va::mail-subject "Campfire ⚡🔥 @ Mon, Grüße"))
  (is equal "Fwd: a b c d e f g h i j k..."
      (va::mail-subject "Fwd: a b c d e f g h i j k l m n o")))

(define-test mail-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Info(~a)"
               '(("read my emails" "MailRead") ("read me my new mail" "MailRead")
                 ("who emailed me" "MailRead") ("what are my latest emails" "MailRead")
                 ("any mail from shannan" "MailFrom(\"shannan\")")
                 ("did shannan email me" "MailFrom(\"shannan\")")
                 ("do i have mail from bob marley" "MailFrom(\"bob marley\")")
                 ("do i have new mail" "Mail")))))
