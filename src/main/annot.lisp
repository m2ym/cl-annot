(in-package :cl-user)

(defpackage cl-annot
  (:use :cl
        :annot.core
        :annot.syntax)
  (:nicknames :annot)
  (:export :enable-annot-syntax
           :defannotation))
