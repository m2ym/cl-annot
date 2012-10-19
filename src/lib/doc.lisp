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
       ((defvar defparameter defconstant defun defmethod defmacro
				deftype)
        (destructuring-bind (def name arg . body)
            definition-form
          `(,def ,name ,arg ,docstring ,@body)))
	   ((defclass)
        (destructuring-bind (def name supers slots . opts)
            definition-form
          `(,def ,name ,supers ,slots
				 ,@(cons `(:documentation ,docstring)
						 opts))))
	   ((defgeneric)
        (destructuring-bind (def name args . opts)
            definition-form
		  `(,def ,name ,args
			 ,@(cons `(:documentation ,docstring)
					 opts))))
       (t (error "Documentation not supported: ~a"
                 definition-form))))
   definition-form))
