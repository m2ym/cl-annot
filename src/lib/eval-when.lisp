(defpackage cl-annot.eval-when
  (:nicknames :annot.eval-when)
  (:use :cl))
(in-package :annot.eval-when)
(annot:enable-annot-syntax)

@export
(defmacro eval-when-compile (&body body)
  `(eval-when (:compile-toplevel) ,@body))

@export
(defmacro eval-when-load (&body body)
  `(eval-when (:load-toplevel) ,@body))

@export
(defmacro eval-when-execute (&body body)
  `(eval-when (:execute) ,@body))

@export
(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))
