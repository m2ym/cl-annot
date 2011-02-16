(in-package :cl-user)

(defpackage cl-annot.eval-when
  (:use :cl
        :cl-annot.util)
  (:nicknames :annot.eval-when)
  (:export :eval-when-compile
           :eval-when-load
           :eval-when-execute
           :eval-always))

(in-package :cl-annot.eval-when)

(defmacro eval-when-compile (&body body)
  `(eval-when (:compile-toplevel) ,@body))
(setf (should-expand-p 'eval-when-compile) t)

(defmacro eval-when-load (&body body)
  `(eval-when (:load-toplevel) ,@body))
(setf (should-expand-p 'eval-when-load) t)

(defmacro eval-when-execute (&body body)
  `(eval-when (:execute) ,@body))
(setf (should-expand-p 'eval-when-execute) t)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))
(setf (should-expand-p 'eval-always) t)
