(in-package :cl-user)
(defpackage cl-annot
  (:nicknames :annot)
  (:use :cl)
  (:import-from :annot.helper
                :defannotation
                :annotation)
  (:import-from :annot.syntax
                :enable-annot-syntax)
  (:export :defannotation
           :annotation
           :enable-annot-syntax))
