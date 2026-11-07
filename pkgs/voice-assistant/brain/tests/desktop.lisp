;;; Desktop and apps, ported from the Rust tests.

(in-package #:va-tests)

(define-test desktop-phrasings
  :parent va-tests
  (with-focus ()
    (loop for (text want) in
          '(("Make the screen brighter." "Brighter") ("Brighter!" "Brighter")
            ("Dim the screen" "Darker")
            ("Set the brightness to 40 percent" "Brightness(40)")
            ("Turn on the night light." "NightLight(true)") ("Night mode off" "NightLight(false)")
            ("Turn off the wifi." "Wifi(false)") ("Turn on the Wi-Fi" "Wifi(true)")
            ("Turn on bluetooth" "Bluetooth(true)") ("Do not disturb." "DoNotDisturb(true)")
            ("Turn off do not disturb" "DoNotDisturb(false)") ("Take a screenshot." "Screenshot")
            ("Screenshot of a region" "ScreenshotRegion") ("Change the wallpaper" "Wallpaper")
            ("Keep the screen on." "Caffeine(true)")
            ("Power saving mode" "PowerProfile(\"power-saver\")") ("Open the launcher" "Launcher")
            ("Clear all notifications." "ClearNotifications") ("Close this window" "CloseWindow")
            ("Fullscreen." "Fullscreen"))
          do (is equal (format nil "Desktop(~a)" want) (shown (va::parse-text text)) "~s" text))))

(define-test app-phrasings
  :parent va-tests
  (with-focus ()
    (flet ((label (text)
             (let ((a (va::parse-text text)))
               (and a (string= (va::command-kind (va::action-command a)) "Launch")
                    (second (nth (first (va::action-fields a)) va::*apps*))))))
      (loop for (text want) in
            '(("Open the browser." "the browser") ("Open Firefox" "the browser")
              ("Launch a terminal" "a terminal") ("Open my email." "your mail")
              ("Open the file manager" "the file manager")
              ("Start the system monitor" "the system monitor")
              ("Open the sound settings" "the sound settings") ("Open Signal." "Signal")
              ("I need a terminal" "a terminal"))
            do (is equal want (label text) "~s" text))
      (false (label "Open the pod bay doors")))))
