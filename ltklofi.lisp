(ql:quickload "ltk")

(defpackage lofi
  (:use :cl
        :ltk))

(in-package :lofi)

(defun run ()
  (uiop:run-program "/bin/brave-browser-stable"))

(defun run ()
  (with-ltk ()
    (let*
        ((f (make-instance 'frame
                           :width 100
                           :height 50))
         (b (make-instance 'entry
                             :text ""
                             :width 100))) 
      (bind b "<KeyPress-Return>"
            (lambda (evt)
              (eval (read-from-string (text b)))))
      (pack f)
      (pack b))))
