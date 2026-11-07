;;; The voice assistant's brain: grammar, skills and dialogue.

(defsystem "voice-assistant-brain"
    :depends-on ("coalton" "alexandria" "com.inuoe.jzon" "sb-bsd-sockets" "sb-introspect")
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "effects")
                 (:file "replies")
                 (:file "commands")
                 (:file "dev")
                 (:module "skills" :pathname "../skills/" :serial t
                          :components ((:file "fun")))))

(defsystem "voice-assistant-brain/tests"
    :depends-on ("voice-assistant-brain" "parachute")
    :pathname "tests/"
    :components ((:file "tests"))
    :perform (test-op (o c) (symbol-call :parachute :test :va-tests)))
