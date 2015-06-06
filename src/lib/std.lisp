(in-package :cl-user)
(defpackage cl-annot.std
  (:nicknames :annot.std)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :export*
           :ignore*
           :ignorable*
		   :dynamic-extent*
		   :declaration*
		   :special*
           :type*
           :ftype*
           :optimize*
           :inline*
           :notinline*))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; simple one-or-more variables
;;;; 

(defun %declare-list-or-symbol (vars sym)
  (if (listp vars)
      `(declare (,sym ,@vars))
      `(declare (,sym ,vars))))

(defannotation ignore* (vars) (:alias ignore :inline t)
  "Shorthand for (DECLARE (IGNORE ...))."
  (%declare-list-or-symbol vars 'ignore))

(defannotation ignorable* (vars) (:alias ignorable :inline t)
  "Shorthand for (DECLARE (IGNORABLE ...))."
  (%declare-list-or-symbol vars 'ignorable))

(defannotation dynamic-extent* (vars) (:alias dynamic-extent :inline t)
  "Shorthand for (DECLARE (DYNAMIC-EXTENT ...))."
  (%declare-list-or-symbol vars 'dynamic-extent))

(defannotation declaration* (vars) (:alias declaration :inline t)
  "Shorthand for (DECLARE (DECLARATION ...))."
  (%declare-list-or-symbol vars 'declaration))

(defannotation special* (vars) (:alias special :inline t)
  "Shorthand for (DECLARE (SPECIAL ...))."
  (%declare-list-or-symbol vars 'special))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; others
;;;;

(defannotation type* (typespec name)
    (:alias type :arity 2 :inline t)
  "Shorthand for (DECLARE (TYPE ...))."
  (if (consp name)
      ;; TODO
      ()
      `(declare (type ,typespec ,name))))

(defannotation ftype* (typespec name)
    (:alias ftype :arity 2 :inline t)
  "Shorthand for (DECLARE (FTYPE ...))."
  (if (consp name)
      ;; TODO
      ()
      `(declare (ftype ,typespec ,name))))

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

(defannotation notinline* (name)
    (:alias notinline :inline t)
  "Shorthand for (DECLARE (NOTINLINE ...))."
  (let ((symbol (definition-form-symbol name))
        (type (definition-form-type name)))
    (if (and symbol
             (member type
                     '(defun defmethod)))
        `(progn
           (declaim (notinline ,symbol))
           ,name)
        `(declare (notinline ,name)))))


