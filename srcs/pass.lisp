(in-package :rolfi)

(defun filter-valid-names (files)
  (mapcan
   (lambda (name)
     (let ((dir-name (car (last (pathname-directory name)))))
       (when dir-name
         (when (char/= (schar dir-name 0) #\.)
           (list name)))))
   files))

(defvar pass-entries)

(defun get-files-in-directories (dir)
  (apply
   #'append
   
   (append
    (mapcar
     
     (lambda (dir-list)
       (mapcar
        
        (lambda (name)
          (namestring (pathname-name name)))
        dir-list))
     
     (setq pass-entries
           
           (mapcar
            #'uiop:directory-files
            (filter-valid-names
             
             (mapcar
              #'namestring
              (uiop:subdirectories dir))))))
    
     (filter-valid-names
      (mapcar #'pathname-name
              (uiop:directory-files dir))))))

(defun get-password-files ()
  (get-files-in-directories "~/.password-store/"))

(defun pass (ent menu f)
  (choose-list-entry
   ent
   menu
   f
   (get-password-files)
   (lambda (entry)
     (uiop:launch-program
      (concatenate 'string "alacritty -e pass -c "
                   ())))))

(defparameter *all-commands* (cons (string 'pass) *all-commands*))
