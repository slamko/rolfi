(defpackage rolfi
  (:use
   :cl
   :ltk)
  (:export
   #:main
   #:choose-list-entry))

(in-package :rolfi)

(defvar app-list nil)
(defparameter *all-commands* (list(string 'lisp-eval)))

(defun get-menu-selection (menu)
  (car (listbox-get-selection menu)))

(defun get-best-matches (str match-list)
  (mapcan
   (lambda (list-str)
     (when (and
            (<= (length str) (length list-str))
            (string-equal str (subseq list-str 0 (length str))) str)
       (list list-str)))
   match-list))

(defun sort-best-match (str match-list)
  (let ((best-match-list
          (get-best-matches str match-list)))
    (append
     best-match-list
     (mapcan
      (lambda (list-str)
        (when (and
               (not (member list-str best-match-list))
               (search str list-str :test #'char-equal))
          (list list-str)))
      match-list))))

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

(defun run-entry-fun (fun entry-id entry-widget)
  (progn
    (funcall fun
             (if entry-id
                 (nth entry-id app-list)
                 (text entry-widget)))))

(defun bind-entry-events (menu entry f initial-app-list eval-fun)
  (bind entry "<KeyPress>"
        (lambda (evt) (entry-update-list menu entry initial-app-list)))

  (bind entry "<KeyPress-Tab>"
    (lambda (evt)
      (autocomplete-selected-entry menu entry f initial-app-list eval-fun)))
 
  (bind entry "<KeyPress-Escape>"
        (lambda (evt)
          (uiop:quit)))

  (bind entry "<KeyPress-Down>"
        (lambda (evt)
          (listbox-select menu 0)
          (focus menu)))

  (bind entry "<KeyPress-Return>"
        (lambda (evt)
         (run-entry-fun eval-fun (get-menu-selection menu) entry))))

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
    
    (bind-entry-events menu entry f initial-app-list eval-fun)
    (focus entry)))

(defun choose-list-entry (entry menu f entry-list eval-fun)
  (let (initial-app-list)
    (setq app-list entry-list)
    (setq initial-app-list app-list)
    
    (listbox-append menu app-list)
    (listbox-select menu 0)

    (bind-entry-events menu entry f initial-app-list eval-fun)
    
    (bind menu "<space>"
          (lambda (evt)
            (autocomplete-selected-entry
             menu entry f initial-app-list eval-fun)))
    
    (bind menu "<KeyPress-Escape>"
          (lambda (evt)
            (uiop:quit)))

    (bind menu "<KeyPress-K>"
          (lambda (evt)
            (listbox-select menu (+ (get-menu-selection menu) 1))))
    
    (bind menu "<KeyPress-J>"
          (lambda (evt)
            (listbox-select menu (- (get-menu-selection menu) 1))))
            
    (bind menu "<KeyPress-Up>"
          (lambda (evt)
            (when (= (get-menu-selection menu) 0)
              (focus entry))))

    (bind menu "<KeyPress-Down>"
      (lambda (evt)
        (when
            (=
             (get-menu-selection menu)
             (- (length app-list) 1))
          (focus entry))))
    
    (bind menu "<KeyPress-Return>"
          (lambda (evt)
           (run-entry-fun eval-fun (get-menu-selection menu) entry)))))

(defun lisp-eval (entry menu f)
  (bind menu "<KeyPress-Return>"
        (lambda (evt)
          (eval (read-from-string (text entry))))))

(defun get-rolfi-fun (fun)
  (concatenate 'string "rolfi::" fun))

(defun run-choose-command (ent menu f)
  (choose-list-entry
   ent
   menu
   f
   *all-commands*
   (lambda (entry)
     (funcall (read-from-string (get-rolfi-fun entry)) ent menu f))))

(defun run (command)
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (entry (make-instance 'entry
                             :master f
                             :width 60))

         (menu (make-instance 'listbox
                              :master f
                              :width 60)))
      (pack f)
      (pack entry)
      (pack menu)
      (focus entry)

      (configure menu :background 'gray)
      (configure entry :background 'gray)

      (funcall command entry menu f))))


(defun main ()
  (run
     (read-from-string
      (let ((command (or (car (uiop:command-line-arguments)) "run-choose-command")))
        (get-rolfi-fun command)))))

