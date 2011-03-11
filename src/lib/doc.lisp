(in-package :cl-user)

(defpackage cl-annot.doc
  (:use :cl
        :annot.core
        :annot.util)
  (:nicknames :annot.doc)
  (:export :doc))

(in-package :annot.doc)

(defmacro doc (docstring form)
  (let ((last (progn-last form)))
    (ecase (definition-type last)
      ((defvar defparameter defconstant defun defmethod defmacro)
       (destructuring-bind (def name arg . body)
           last
         (progn-replace-last
          `(,def ,name ,arg ,docstring ,@body)
          form)))
      (t (error "Documentation not supported: ~a" last)))))
(setf (annotation-narg 'doc) 2)
