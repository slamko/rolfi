(load "~/quicklisp/setup.lisp")

(ql:quickload "ltk")

(defpackage rolfi
  (:use :cl
        :ltk))

(in-package :rolfi)

(defun get-menu-selection (menu)
  (car (listbox-get-selection menu)))

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
			(setq unique-bin-names
                  (cons bin-name unique-bin-names))))))

(defun get-best-matches (str match-list)
  (mapcan
   (lambda (list-str)
     (when (and
            (<= (length str) (length list-str))
            (string= str (subseq list-str 0 (length str))) str)
       (list list-str)))
   match-list))

(defun sort-best-match (str match-list)
  (let ((best-match-list
          (get-best-matches str match-list)))
    
    (append
     (reverse best-match-list)
     (reverse
      (mapcan
       (lambda (list-str)
         (when (and
                (not (member list-str best-match-list))
                (search str list-str))
           (list list-str)))
       match-list)))))

(defun bind-entry-events (menu entry app-list initial-app-list)
  (bind entry "<KeyPress>"
          (lambda (evt)
            (listbox-clear menu)
            (listbox-append
             menu
             (setq app-list
                   (if (= (length (text entry)) 0)
                       initial-app-list
                      (sort-best-match (text entry) initial-app-list))))
            (listbox-select menu 0)))

    (bind entry "<KeyPress-Down>"
          (lambda (evt)
            (focus menu)
            (listbox-select menu 0)))
    

    (bind entry "<KeyPress-Return>"
          (lambda (evt)
            (uiop:launch-program
             (uiop:split-string (nth (get-menu-selection menu) app-list)))
            (uiop:quit))))

(defun app-launcher (entry menu f)
  (let* ((app-list (unique-bins (all-bins (get-bin-directories))))
         (initial-app-list app-list))
    (listbox-append menu app-list)
    (listbox-select menu 0)

    (bind-entry-events menu entry app-list initial-app-list)
    
    (bind menu "<space>"
          (lambda (evt)
            (ltk:pack-forget menu)
            (ltk:pack-forget entry)
            (listbox-append menu app-list)
            
            (setq entry (make-instance 'entry
                                       :master f
                                       :text (nth (get-menu-selection menu) app-list)
                                       :width 60))

            (pack entry)
            (pack menu)
            (ltk:configure f
                     :borderwidth 3
                     :height 50
                     :relief
                     :sunken)
            (bind-entry-events menu entry app-list initial-app-list)
            (focus entry)))
            
    (bind menu "<KeyPress-Up>"
          (lambda (evt)
            (when (= (get-menu-selection menu) 0)
              (focus entry))))
    
    (bind menu "<KeyPress-Return>"
          (lambda (evt)
            (uiop:launch-program
             (uiop:split-string (nth (get-menu-selection menu) app-list)))
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
      (app-launcher entry menu f))))

(run)
