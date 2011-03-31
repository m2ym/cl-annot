(defpackage cl-annot.doc
  (:use :cl
        :annot.util
        :annot.core)
  (:nicknames :annot.doc)
  (:export :doc))
(in-package :annot.doc)

(defannotation doc (docstring form)
    (:arity 2)
  (let ((last (progn-form-last form)))
    (ecase (definition-form-type last)
      ((defvar defparameter defconstant defun defmethod defmacro)
       (destructuring-bind (def name arg . body)
           last
         (progn-form-replace-last
          `(,def ,name ,arg ,docstring ,@body)
          form)))
      (t (error "Documentation not supported: ~a" last)))))
