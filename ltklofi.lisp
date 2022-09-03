(ql:quickload "ltk")

(defpackage lofi
  (:use :cl
        :ltk))

(in-package :lofi)

(defun run-pcman ()
  (uiop:run-program "/bin/pcmanfm"))

(defun run ()
  (with-ltk ()
    (let*
        ((text "(run-pcman)")
           (b (make-instance 'entry
                             :text text
                             :width 100))) 
      (princ (eval (read-from-string (text b))))
      (pack b))))
