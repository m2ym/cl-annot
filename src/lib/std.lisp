(in-package :cl-user)

(defpackage cl-annot.std
  (:use :cl
        :macro-utils
        :annot.core)
  (:nicknames :annot.std)
  (:export :export*
           :ignore*
           :ignorable*
           :type*))

(in-package :annot.std)

(defmacro export* (form)
  "Export the definition symbol of FORM."
  (let* ((last (progn-form-last form))
         (symbol (definition-form-symbol last)))
    `(progn
       (export ',symbol)
       ,form)))
(setf (annotation-real 'export) 'export*)

(defmacro ignore* (vars)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))
(setf (annotation-real 'ignore) 'ignore*
      (annotation-inline-p 'ignore*) t)

(defmacro ignorable* (vars)
  "Shorthand of (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))
(setf (annotation-real 'ignorable) 'ignorable*
      (annotation-inline-p 'ignorable*) t)

(defmacro type* (typespec)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,@typespec)))
(setf (annotation-real 'type) 'type*
      (annotation-inline-p 'type*) t)
