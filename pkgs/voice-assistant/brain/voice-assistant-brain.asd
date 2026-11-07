;;; The voice assistant's brain: grammar, skills and dialogue.

(defsystem "voice-assistant-brain"
    :depends-on ("coalton" "alexandria" "trivia" "com.inuoe.jzon" "sb-bsd-sockets" "sb-introspect")
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "defn")
                 (:file "effects")
                 (:file "replies")
                 (:file "types")
                 (:file "adt")
                 (:file "words")
                 (:file "duration")
                 (:file "clock")
                 (:file "date")
                 (:file "holiday")
                 (:file "grammar")
                 (:file "commands")
                 (:file "dispatch")
                 (:file "dialogue")
                 (:file "fuzzy")
                 (:file "dev")
                 (:module "skills" :pathname "../skills/" :serial t
                          :components ((:file "timer")
                                       (:file "system")
                                       (:file "volume")
                                       (:file "media")
                                       (:file "desktop")
                                       (:file "apps")
                                       (:file "info")
                                       (:file "fun")
                                       (:file "kagi")
                                       (:file "core")))))

(defsystem "voice-assistant-brain/tests"
    :depends-on ("voice-assistant-brain" "parachute")
    :pathname "tests/"
    :serial t
    :components ((:file "tests")
                 (:file "parsing")
                 (:file "grammar")
                 (:file "dispatch")
                 (:file "fun")
                 (:file "timer")
                 (:file "dialogue")
                 (:file "system")
                 (:file "media")
                 (:file "desktop")
                 (:file "info")
                 (:file "kagi"))
    :perform (test-op (o c) (symbol-call :parachute :test :va-tests)))
