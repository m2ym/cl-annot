(defpackage cl-annot.std
  (:use :cl
        :annot.util
        :annot.core)
  (:nicknames :annot.std)
  (:export :export*
           :ignore*
           :ignorable*
           :type*))

(in-package :annot.std)

(defannotation export* (form)
    (:alias export)
  "Export the definition symbol of FORM."
  (let* ((last (progn-form-last form))
         (symbol (definition-form-symbol last)))
    `(progn
       (export ',symbol)
       ,form)))

(defannotation ignore* (vars)
    (:alias ignore :inline t)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))

(defannotation ignorable* (vars)
    (:alias ignorable :inline t)
  "Shorthand of (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))

(defannotation type* (typespec)
    (:alias type :inline t)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,@typespec)))
