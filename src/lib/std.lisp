(in-package :cl-user)

(defpackage cl-annot.std
  (:use :cl
        :annot.core
        :annot.util)
  (:nicknames :annot.std)
  (:export :export*
           :ignore*
           :ignorable*
           :type*))

(in-package :annot.std)

(defmacro export* (form)
  "Export the definition symbol of FORM."
  `(export ',(definition-symbol form)))
(setf (toplevel-annotation-p 'export*) t
      (real-annotation 'export) 'export*)

(defmacro ignore* (vars)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))
(setf (annotation-expand-p 'ignore*) t
      (real-annotation 'ignore) 'ignore*)

(defmacro ignorable* (vars)
  "Shorthand of (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))
(setf (annotation-expand-p 'ignorable*) t
      (real-annotation 'ignorable) 'ignorable*)

(defmacro type* (type-specs)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,type-specs)))
(setf (annotation-expand-p 'type*) t
      (real-annotation 'type) 'type*)
