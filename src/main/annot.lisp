(defpackage cl-annot
  (:nicknames :annot)
  (:use :cl)
  (:import-from :annot.helper
                :defannotation
                :annotation)
  (:import-from :annot.syntax
                :syntax
                :enable-annot-syntax)
  (:export :defannotation
           :annotation
           :syntax
           :enable-annot-syntax))
