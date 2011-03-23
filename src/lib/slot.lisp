(defpackage cl-annot.slot
  (:use :cl
        :annot.util
        :annot.core)
  (:nicknames :annot.slot)
  (:export :required))

(in-package :annot.slot)

(defannotation required (slot-specifier)
    (:inline t)
  (destructuring-bind (slot-name . options)
      (if (consp slot-specifier)
          slot-specifier
          (list slot-specifier))
    (cond
      ((not (eq (getf options :initform 'not-found) 'not-found))
       (error "Required slot ~S must not have :initform" slot-name))
      ((eq (getf options :initarg 'not-found) 'not-found)
       (error "Required slot ~S must have :initarg" slot-name))
      (t
       (setf (getf options :initform)
             `(error ,(format nil "Must supply ~S" (getf options :initarg))))))
    (cons slot-name options)))
