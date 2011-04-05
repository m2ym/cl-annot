(defpackage cl-annot.std
  (:nicknames :annot.std)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :export*
           :ignore*
           :ignorable*
           :type*))
(in-package :annot.std)

(defannotation export* (definition-form)
    (:alias export)
  "Export the definition symbol of DEFINITION-FORM."
  (let ((name (definition-form-symbol definition-form)))
    (if name
        `(progn
           (export ',name)
           ,definition-form)
        definition-form)))

(defannotation ignore* (vars)
    (:alias ignore :inline t)
  "Shorthand for (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))

(defannotation ignorable* (vars)
    (:alias ignorable :inline t)
  "Shorthand for (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))

(defannotation type* (typespec)
    (:alias type :inline t)
  "Shothand for (DECLARE (TYPE ...))."
  `(declare (type ,@typespec)))
