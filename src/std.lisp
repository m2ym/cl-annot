(in-package :cl-user)

(defpackage cl-annot.std
  (:use :cl
        :cl-annot.util)
  (:nicknames :annot.std)
  (:export :export*
           :ignore*
           :type*))

(in-package :cl-annot.std)

(defun export* (object)
  "Export the reference symbol of OBJECT."
  (export (reference-symbol object)))
(setf (should-expand-p 'export*) t
      (annotation-macro 'export) 'export*)

(defmacro ignore* (vars)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))
(setf (should-expand-p 'ignore*) t
      (annotation-macro 'ignore) 'ignore*)

(defmacro type* (type-specs)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,type-specs)))
(setf (should-expand-p 'type*) t
      (annotation-macro 'type) 'type*)
