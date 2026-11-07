(defpackage #:va
  (:use #:cl)
  (:shadow #:number)
  (:local-nicknames (#:a #:alexandria) (#:jzon #:com.inuoe.jzon))
  (:export #:run #:fake-effects #:effects
           #:defreplies #:pick #:*late*
           #:defcommand #:defskill #:commands #:command-name #:command-templates
           #:command-examples #:command-skill
           #:serve #:check #:reload #:main))
