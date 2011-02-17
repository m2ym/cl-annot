(in-package :cl-user)

(defpackage cl-annot.std
  (:use :cl
        :cl-annot.util)
  (:nicknames :annot.std)
  (:export :export*
           :ignore*
           :type*))

(in-package :cl-annot.std)

(defmacro export* (form)
  "Export the reference symbol of FORM."
  (let ((symbol (cadr form)))
    (if (and (consp symbol)
             (eq (car symbol) 'setf))
        (setf symbol (cadr symbol)))
    `(progn
       ,form
       (export ',symbol)
       ',symbol)))
(setf (should-expand-p 'export*) t
      (annotation 'export) 'export*)

(defmacro ignore* (vars)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))
(setf (should-expand-p 'ignore*) t
      (annotation 'ignore) 'ignore*)

(defmacro ignorable* (vars)
  "Shorthand of (DECLARE (IGNORABLE ...))."
  (if (listp vars)
      `(declare (ignorable ,@vars))
      `(declare (ignorable ,vars))))
(setf (should-expand-p 'ignorable*) t
      (annotation 'ignorable) 'ignorable*)

(defmacro type* (type-specs)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,type-specs)))
(setf (should-expand-p 'type*) t
      (annotation 'type) 'type*)
