(in-package :cl-user)

(defpackage cl-annot-asd
  (:use :cl :asdf))

(in-package cl-annot-asd)

(defsystem cl-annot
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria)
  :components ((:module "src"
                :serial t
                :components ((:module "main"
                              :serial t
                              :components ((:file "utils")
                                           (:file "core")
                                           (:file "expand")
                                           (:file "syntax")
                                           (:file "api")
                                           (:file "annot")))
                             (:module "lib"
                              :serial t
                              :components ((:file "std")
                                           (:file "eval-when")
                                           (:file "doc")
                                           (:file "class")
                                           (:file "slot")))))))
