(defpackage cl-annot.std
  (:nicknames :annot.std)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :export*
           :ignore*
           :ignorable*
           :type*
           :optimize*
           :inline*))
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

(defannotation type* (typespec name)
    (:alias type :arity 2 :inline t)
  "Shorthand for (DECLARE (TYPE ...))."
  (if (consp name)
      ;; TODO
      ()
      `(declare (type ,typespec ,name))))

(defannotation optimize* (quality)
    (:alias optimize :inline t)
  "Shorthand for (DECLARE (OPTIMIZE ...))."
  `(declare (optimize ,quality)))

(defannotation inline* (name)
    (:alias inline :inline t)
  "Shorthand for (DECLARE (INLINE ...))."
  (let ((symbol (definition-form-symbol name))
        (type (definition-form-type name)))
    (if (and symbol
             (member type
                     '(defun defmethod)))
        `(progn
           (declaim (inline ,symbol))
           ,name)
        `(declare (inline ,name)))))
