(in-package :cl-user)

(defpackage cl-annot-test
  (:use :cl
        :cl-test-more
        :cl-annot
        :cl-annot.util
        :cl-annot.std
        :cl-annot.eval-when))

(in-package :cl-annot-test)

(enable-annot-syntax)

(is (function-name (defgeneric g ()))
    'g
    "function-name")
(ok (method-function (defmethod m ()))
    "method-function")
(is (method-name (defmethod m ()))
    'm
    "method-name")
(is (reference-symbol (defun (setf f) ()))
    'f
    "reference-symbol for setf-function")
(is (reference-symbol 'f)
    'f
    "reference-symbol for symbol")
(is (reference-symbol (defclass c () ()))
    'c
    "reference-symbol for class")
(is (reference-symbol (defgeneric g ()))
    'g
    "reference-symbol for generic-function")
(is (reference-symbol (defmethod m ()))
    'm
    "reference-symbol for method")
(is @1+ 1
    1
    "expression annotation")
(is @or 1
    1
    "macro annotation")
(is @(lambda (x) (1+ x)) 1
    1
    "lambda form annotation")
(is @export (defun x ())
    'x
    "export annotation")
(is (cadr (multiple-value-list (find-symbol "X")))
    :external
    "exported?")
(is '@ignore v
    '(declare (ignore v))
    "ignore declaration")
(is '@type (integer v)
    '(declare (type (integer v)))
    "type declaration")
(is '@eval-when-compile 1
    '(eval-when (:compile-toplevel) 1)
    "eval-when-compile")
(is '@eval-when-load 1
    '(eval-when (:load-toplevel) 1)
    "eval-when-load")
(is '@eval-when-execute 1
    '(eval-when (:execute) 1)
    "eval-when-execute")
(is '@eval-always 1
    '(eval-when (:compile-toplevel :load-toplevel :execute) 1)
    "eval-always")

(finalize)
