(defpackage cl-annot
  (:nicknames :annot)
  (:use :cl)
  (:import-from :annot.helper
                :defannotation
                :annotation)
  (:import-from :annot.syntax
                :annot-syntax
                :enable-annot-syntax)
  (:export :defannotation
           :annotation
           :annot-syntax
           :enable-annot-syntax))
