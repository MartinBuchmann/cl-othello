;;;; cl-othello.asd

(asdf:defsystem #:cl-othello
  :description "Describe cl-othello here"
  :author "Martin Buchmann <Martin.Buchmann@gmail.com>"
  :license "Public Domain"
  :depends-on (#:lisp-unit
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "cl-othello")))

