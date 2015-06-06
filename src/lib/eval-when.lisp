(in-package :cl-user)
(defpackage cl-annot.eval-when
  (:nicknames :annot.eval-when)
  (:use :cl)
  (:export :eval-when-compile
           :eval-when-load
           :eval-when-execute
           :eval-always))
(in-package :annot.eval-when)

(defmacro eval-when-compile (&body body)
  `(eval-when (:compile-toplevel) ,@body))

(defmacro eval-when-load (&body body)
  `(eval-when (:load-toplevel) ,@body))

(defmacro eval-when-execute (&body body)
  `(eval-when (:execute) ,@body))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))
