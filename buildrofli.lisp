(ql:quickload :ltk)

(load "rolfi.asd")
(asdf:load-system :rolfi)

(sb-ext:save-lisp-and-die "rolfi" :toplevel #'rolfi:main :executable t)

(quit)
