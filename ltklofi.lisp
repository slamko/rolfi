(load "~/quicklisp/setup.lisp")

(ql:quickload "ltk")

(defpackage rolfi
  (:use :cl
        :ltk))

(in-package :rolfi)

(defun launch-program (str)
  (uiop:launch-program
   (uiop:split-string str)))

(defun eval-entry (str)
  (eval (read-from-string str)))
  
(defun run ()
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (entry (make-instance 'entry
                             :text ""
                             :master f
                             :width 60
                             :takefocus t))) 
      (bind entry "<KeyPress-Return>"
            (lambda (evt)
              (launch-program (text entry))
              (uiop:quit)))
      (pack f)
      (pack entry)
      (ltk:configure f
                     :borderwidth 3
                     :relief
                     :sunken))))

(run)
