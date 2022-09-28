(in-package :rolfi)

(defun powerctl (ent menu f)
  (choose-list-entry
   ent
   menu
   f
   '("sleep" "shutdown" "reboot")
   (lambda (entry &rest args)
     (cond ((string= entry "sleep")
            (uiop:launch-program "loginctl suspend"))
           ((string= entry "shutdown")
            (uiop:launch-program "loginctl poweroff"))
           ((string= entry "reboot")
            (uiop:launch-program "loginctl reboot")))
     (uiop:quit))))

(defparameter *all-commands* (cons (string 'powerctl) *all-commands*))

