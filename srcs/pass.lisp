(in-package :rolfi)

(defun filter-valid-names (files pathname-fun)
  (mapcan
   (lambda (name)
     (let ((dir-name (funcall pathname-fun name)))
       (when dir-name
         (when (char/= (schar dir-name 0) #\.)
           (list name)))))
   files))

(defvar pass-entries)

(defun get-dir-files-rec (dir)
  (append
   (filter-valid-names
    (uiop:directory-files dir)
    'pathname-name)

   (let (subdir-files)
     (dolist
         (subdir
          (filter-valid-names
           (uiop:subdirectories dir)
           (lambda (path) (car (last (pathname-directory path))))) subdir-files)

       (setq subdir-files (append subdir-files (get-dir-files-rec subdir)))))))

(defun get-files-in-directories (dir)
  (mapcar
   (lambda (path)
     (apply
      #'format
      (append
       (list 'nil "~{~A~^, ~}")
       (let ((path-dirs
               (cdr
                (member (car
                         (last (pathname-directory dir)))
                        (pathname-directory path) :test #'string=))))
         (if (listp path-dirs)
             path-dirs
             (list path-dirs)))
       (list (pathname-name path)))))
   (get-dir-files-rec dir)))

(defun get-password-files ()
  (get-files-in-directories "~/.password-store/"))

(defun pass (ent menu f)
  (let ((pass-files (get-password-files)))
    (choose-list-entry
     ent
     menu
     f
     pass-files
     (lambda (entry &rest args)
       (uiop:launch-program
        (concatenate 'string
                     "alacritty -e pass -c "
                     (car (member entry (car args)))))))))

(defparameter *all-commands* (cons (string 'pass) *all-commands*))
