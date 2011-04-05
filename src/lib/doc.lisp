(defpackage cl-annot.doc
  (:nicknames :annot.doc)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :doc))
(in-package :annot.doc)

(defannotation doc (docstring definition-form)
    (:arity 2)
  "Add DOCSTRING documentation for DEFINITION-FORM."
  (progn-form-replace-last
   (lambda (definition-form)
     (case (definition-form-type definition-form)
       ((defvar defparameter defconstant defun defmethod defmacro)
        (destructuring-bind (def name arg . body)
            definition-form
          `(,def ,name ,arg ,docstring ,@body)))
       (t (error "Documentation not supported: ~a"
                 definition-form))))
   definition-form))
