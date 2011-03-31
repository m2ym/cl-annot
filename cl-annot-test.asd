(in-package :cl-user)

(defpackage cl-annot-test-asd
  (:use :cl :asdf))

(in-package :cl-annot-test-asd)

(defsystem cl-annot-test
  :depends-on (:cl-test-more :cl-annot)
  :components ((:module "t"
                :serial t
                :components ((:file "annot")))))
