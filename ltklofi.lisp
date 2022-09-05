(load "~/quicklisp/setup.lisp")

(ql:quickload "ltk")

(defpackage rolfi
  (:use :cl
        :ltk))

(in-package :rolfi)

(defun get-bin-directories ()
  (let ((path-var (uiop:getenv "PATH")))
	(when path-var
	  (mapcar
       (lambda (var) (concatenate 'string var "/"))
       (uiop:split-string path-var :separator ":")))))

(defun all-bins (bin-directories)
  (let (all-bin-names)
	(dolist (bin-path bin-directories all-bin-names)
	  (if t
		  (setq all-bin-names
                (append
                 (mapcar 'pathname-name
                         (uiop:directory-files bin-path)) all-bin-names))))))

(defun unique-bins (bins)
  (let (unique-bin-names)
	(dolist (bin-name bins unique-bin-names)
		(if (not (member bin-name unique-bin-names :test 'string=))
			(setq unique-bin-names (append (list bin-name) unique-bin-names))))))

(unique-bins '("some" "some"))

(defun app-launcher (entry menu)
  (let ((app-list (unique-bins (all-bins (get-bin-directories)))))
    (listbox-append menu app-list)
    (listbox-select menu 0)
    
    (bind entry "<KeyPress>"
          (lambda (evt)
            (listbox-clear menu)
            (listbox-append
             menu
             (setq app-list
                   (remove nil
                           (mapcar
                            (lambda (str)
                              (when
                                  (search
                                   (text entry) str)
                          str))
                            app-list))))

            (listbox-select menu 0)))
    
    (bind entry "<KeyPress-Return>"
          (lambda (evt)
            (uiop:launch-program
             (uiop:split-string (nth (car (listbox-get-selection menu)) app-list)))
            (uiop:quit)))))
  
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
