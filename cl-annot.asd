(in-package :cl-user)

(defpackage cl-annot-asd
  (:use :cl :asdf))

(in-package cl-annot-asd)

(defsystem cl-annot
    :version "0.1"
    :author "Tomohiro Matsuyama"
    :license "LLGPL"
    :components ((:file "annot")))
