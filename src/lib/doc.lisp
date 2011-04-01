(defpackage cl-annot.doc
  (:nicknames :annot.doc)
  (:use :cl
        :annot.util
        :annot.api)
  (:export :doc))
(in-package :annot.doc)
(annot:enable-annot-syntax)

@export
@annotation (:arity 2)
(defmacro doc (docstring definition-form)
  "Add DOCSTRING documentation for DEFINITION-FORM."
  (let ((last (progn-form-last definition-form)))
    (ecase (definition-form-type last)
      ((defvar defparameter defconstant defun defmethod defmacro)
       (destructuring-bind (def name arg . body)
           last
         (progn-form-replace-last
          `(,def ,name ,arg ,docstring ,@body)
          definition-form)))
      (t (error "Documentation not supported: ~a" last)))))
