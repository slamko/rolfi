(asdf:defsystem "rolfi"
  :author "Viacheslav Chepelyk-Kozhin"
  :description "Lisp driven apllication launcher and more"
  :version "0.1.2"
  :license "GPLv3"
  :depends-on (:ltk :uiop)
  :components ((:module "src"
                :components
                ((:file "rolfi")))))
