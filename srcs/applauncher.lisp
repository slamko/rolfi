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
                 all-bin-names
                 (mapcar 'pathname-name
                         (uiop:directory-files bin-path))))))))

(defun unique-bins (bins)
  (let (unique-bin-names)
	(dolist (bin-name bins unique-bin-names)
		(if (not (member bin-name unique-bin-names :test 'string=))
			(setq unique-bin-names
                  (cons bin-name unique-bin-names))))))

(defun run-entry (entry)
  (uiop:launch-program entry))

(defun app-launcher (entry menu f)
  (choose-list-entry
   entry
   menu
   f
   (remove-duplicates (all-bins (get-bin-directories)) :test #'string=)
   'run-entry))

