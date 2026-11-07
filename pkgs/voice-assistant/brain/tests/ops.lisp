;;; Ported from ops.rs.

(in-package #:va-tests)

(define-test ops-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "~a"
               '(
                 ("Is eve up?" "Ops(HostUp(0))")
                 ("Is the server down?" "Ops(HostUp(0))")
                 ("Can you ping blob 64?" "Ops(HostUp(2))")
                 ("Is turingmachine online?" "Ops(HostUp(4))")
                 ("Is the internet down?" "Ops(Internet)")
                 ("Find my phone." "Ops(FindPhone)")
                 ("Connect my headphones." "Ops(Bluetooth { device: 0, connect: true })")
                 ("Switch the audio to the earbuds" "Ops(Bluetooth { device: 1, connect: true })")
                 ("Disconnect the mouse" "Ops(Bluetooth { device: 2, connect: false })")
                 ("Where's my phone?" "Ops(FindPhone)")
                 ("I can't find my phone!" "Ops(FindPhone)")
                 ("Am I online?" "Ops(Internet)")
                 ("What's my IP address?" "Ops(Ip)")
                 ("How much disk space is left?" "Ops(Disk)")
                 ("How much memory is free?" "Ops(Memory)")
                 ("Why is my computer so slow?" "Ops(Cpu)")
                 ("What's eating my CPU?" "Ops(Cpu)")))))

(define-test ops-parsing
  :parent va-tests
  (let ((raw (format nil "Device E0:49:ED:04:B7:B4 Audeze Maxwell BT~%~
                          Device 0C:53:B7:B5:B2:49 Beats Flex~%")))
    (is equal "0C:53:B7:B5:B2:49" (va::bt-mac raw "beats"))
    (is equal "E0:49:ED:04:B7:B4" (va::bt-mac raw "Audeze"))
    (false (va::bt-mac raw "MX Master")))
  (let ((raw (format nil "MemTotal:       32000000 kB~%MemFree:  100 kB~%~
                          MemAvailable:   16000000 kB~%")))
    (is = 32000000 (va::meminfo raw "MemTotal"))
    (is = 16000000 (va::meminfo raw "MemAvailable"))
    (false (va::meminfo raw "Mem"))))

(define-test ops-follow-ups
  :parent va-tests
  (with-focus (:previous (va::parse-text "is eve up"))
    (is equal "Ops(HostUp(1))" (parsed "and eva?")))
  (with-focus (:previous (va::parse-text "disconnect the mouse"))
    (is equal "Ops(Bluetooth { device: 0, connect: false })" (parsed "and the headphones?"))))
