;;;; bullrender.asd

(asdf:defsystem #:bullrender
  :description "Describe bullrender here"
  :author "Alexander White"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "bullrender")))

(asdf:defsystem #:bullrender-test
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (#:bullrender #:fiveam))

