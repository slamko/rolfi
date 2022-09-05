(load "~/quicklisp/setup.lisp")

(ql:quickload "ltk")

(defpackage rolfi
  (:use :cl
        :ltk))

(in-package :rolfi)

(defun app-launcher (entry menu)
  (let ((app-list '("pcmanfm" "alacritty")))
    (listbox-append menu ')
    
    (bind entry "<KeyPress>"
          (lambda (evt)
            (listbox-select menu
                            (remove nil
                                    (mapcar (lambda (str) (search (text entry) str)) app-list)))))
    
    (bind entry "<KeyPress-Return>"
          (lambda (evt)
            (uiop:launch-program
             (uiop:split-string (text entry)))
            (uiop:quit))))
  
  (defun lisp-eval (entry menu)
  (eval (read-from-string (text entry))))
  
(defun run ()
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (entry (make-instance 'entry
                             :text ""
                             :master f
                             :width 60
                             :takefocus t))
         (menu (make-instance 'listbox
                              :master f
                              :width 60))) 
      (pack f)
      (pack entry)
      (pack menu)
      (focus entry)
      (ltk:configure f
                     :borderwidth 3
                     :height 50
                     :relief
                     :sunken)
      (app-launcher entry menu))))

(run)
