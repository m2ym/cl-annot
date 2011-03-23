(defpackage cl-annot.slot
  (:use :cl
        :annot.util
        :annot.core)
  (:nicknames :annot.slot)
  (:export :initarg
           :required))

(in-package :annot.slot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (string)
    (intern (string string) :keyword)))

(defannotation initarg (slot-specifier)
    (:inline t)
  (destructuring-bind (slot-name . options)
      (if (consp slot-specifier)
          slot-specifier
          (list slot-specifier))
    (if (getf options :initarg)
        (error "~S must not have :initarg" slot-name)
        (setf (getf options :initarg)
              (make-keyword slot-name)))
    (cons slot-name options)))

(defannotation required (slot-specifier)
    (:inline t)
  (destructuring-bind (slot-name . options)
      (if (consp slot-specifier)
          slot-specifier
          (list slot-specifier))
    (cond
      ((not (eq (getf options :initform 'not-found) 'not-found))
       (error "Required slot ~A must not have :initform" slot-name))
      ((eq (getf options :initarg 'not-found) 'not-found)
       (error "Required slot ~A must have :initarg" slot-name))
      (t
       (setf (getf options :initform)
             `(error ,(format nil "Must supply ~S" (getf options :initarg))))))
    (cons slot-name options)))
