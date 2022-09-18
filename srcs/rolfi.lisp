(defpackage rolfi
  (:use :cl
   :ltk)
  (:export #:main))

(in-package :rolfi)

(defvar app-list nil)

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

(defun entry-update-list (menu entry initial-app-list)
  (progn
    (listbox-clear menu)
    (listbox-append
     menu
     (setq app-list
           (if (= (length (text entry)) 0)
               initial-app-list
               (sort-best-match (text entry) initial-app-list))))
    (listbox-select menu 0)))

(defun bind-entry-events (menu entry initial-app-list eval-fun)
  (bind entry "<KeyPress>"
        (lambda (evt) (entry-update-list menu entry initial-app-list)))
 
  (bind entry "<KeyPress-Escape>"
        (lambda (evt)
          (uiop:quit)))

  (bind entry "<KeyPress-Down>"
        (lambda (evt)
          (focus menu)
          (listbox-select menu 0)))

  (bind entry "<KeyPress-Return>"
        (lambda (evt)
          (funcall eval-fun
                   (nth (get-menu-selection menu) app-list))
          (uiop:quit))))

(defun autocomplete-selected-entry (menu entry f initial-app-list eval-fun)
  (progn
    (ltk:pack-forget entry)
    (listbox-select menu 0)
    (ltk:pack-forget menu)
    
    (setq entry (make-instance 'entry
                               :master f
                               :text (nth (get-menu-selection menu) app-list)
                               :width 60))
    
    (pack entry)
    (pack menu)
    (entry-update-list menu entry initial-app-list)
    
    (ltk:configure f
                   :borderwidth 3
                   :height 50
                   :relief
                   :sunken)
    
    (bind-entry-events menu entry initial-app-list eval-fun)
    (focus entry)))

(defun choose-list-entry (entry menu f entry-list eval-fun)
  (let (initial-app-list)
    (setq app-list entry-list)
    (setq initial-app-list app-list)
    
    (listbox-append menu app-list)
    (listbox-select menu 0)

    (bind-entry-events menu entry initial-app-list eval-fun)
    
    (bind menu "<space>"
          (lambda (evt)
            (autocomplete-selected-entry menu entry f initial-app-list eval-fun)))
       (bind menu "<KeyPress-Escape>"
          (lambda (evt)
            (uiop:quit)))
            
    (bind menu "<KeyPress-Up>"
          (lambda (evt)
            (when (= (get-menu-selection menu) 0)
              (focus entry))))
    
    (bind menu "<KeyPress-Return>"
          (lambda (evt)
            (funcall eval-fun (nth (get-menu-selection menu) app-list))
            (uiop:quit)))))

(defun run-entry (entry)
  (uiop:launch-program entry))

(defun power-control (ent menu f)
  (choose-list-entry
   ent
   menu
   f
   '("sleep" "shutdown" "reboot")
   (lambda (entry)
     (cond ((string= entry "sleep")
            (uiop:launch-program "loginctl suspend"))
           ((string= entry "shutdown")
            (uiop:launch-program "loginctl poweroff"))
           ((string= entry "reboot")
            (uiop:launch-program "loginctl reboot"))))))

(defun app-launcher (entry menu f)
  (choose-list-entry
   entry
   menu
   f
   (unique-bins (all-bins (get-bin-directories)))
   'run-entry))

(defun lisp-eval (entry menu f)
  (eval (read-from-string (text entry))))
  
(defun run (command)
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (entry (make-instance 'entry
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
      (configure menu :background 'gray)
      (configure entry :background 'gray)
      (ltk:configure f
                     :borderwidth 3
                     :height 50
                     :relief
                     :sunken)
      (funcall command entry menu f))))

(defun main ()
  (run
     (read-from-string
      (or
       (concatenate 'string "rolfi::" (car (uiop:command-line-arguments)))
        "app-launcher"))))

