(defpackage cl-annot.std
  (:nicknames :annot.std)
  (:use :cl
        :annot.util
        :annot.api)
  (:export :export*
           :ignore*
           :ignorable*
           :type*))
(in-package :annot.std)
(annot:enable-annot-syntax)

@annotation (:alias export)
(defmacro export* (definition-form)
  "Export the definition symbol of DEFINITION-FORM."
  (let ((name (definition-form-symbol
                  (progn-form-last definition-form))))
    `(progn
       (export ',name)
       ,definition-form)))

@annotation (:alias ignore :inline t)
(defmacro ignore* (vars)
  "Shorthand for (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))

@annotation (:alias ignorable :inline t)
(defmacro ignorable* (vars)
  "Shorthand for (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))

@annotation (:alias type :inline t)
(defmacro type* (typespec)
  "Shothand for (DECLARE (TYPE ...))."
  `(declare (type ,@typespec)))
