(defpackage cl-annot
  (:nicknames :annot)
  (:use :cl)
  (:import-from :annot.api
                :defannotation
                :annotation
                :enable-annot-syntax)
  (:export :defannotation
           :annotation
           :enable-annot-syntax))
