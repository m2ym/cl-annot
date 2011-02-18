(in-package :cl-user)

(defpackage cl-annot.doc
  (:use :cl
        :annot.core
        :annot.util)
  (:nicknames :annot.doc)
  (:export :doc))

(in-package :annot.doc)

(defmacro doc (docstring form)
  (ecase (definition-type form)
    ((defvar defparameter defconstant defun defmacro)
     (destructuring-bind (def name arg . rest)
         form
       `(,def ,name ,arg ,docstring ,@rest)))
    (t (error "Documentation not supported for ~a" form))))
(setf (annotation-narg 'doc) 2)
