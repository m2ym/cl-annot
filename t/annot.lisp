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

(defun symbol-status (name)
  (cadr (multiple-value-list (find-symbol (string-upcase name)))))

(is @1+ 1
    1
    "expression")
(is-expand @1+ 1
           (let (($ 1)) (1+ $) $)
           "expression expansion")
(is @or 1
    1
    "macro")
(is '@or 1
    '(or 1)
    "macro expansion")
(is @(lambda (x) (1+ x)) 1
    1
    "lambda form")
(is @export (defun x ())
    'x
    "export function")
(is (symbol-status :x)
    :external
    "function exported?")
(is @export (defun (setf s) ())
    's
    "export setf function")
(is (symbol-status :s)
    :external
    "setf function exported?")
(is @export (defgeneric g ())
    'g
    "export generic function")
(is (symbol-status :g)
    :external
    "generic function exported?")
(is @export (defmethod m ())
    'm
    "export method")
(is (symbol-status :m)
    :external
    "method exported?")
(is @export (defmethod c ())
    'c
    "export class")
(is (symbol-status :c)
    :external
    "class exporteded?")
(is-expand @export (defun x ())
           (progn (defun x ()) (export 'x))
           "export expansion")
(is '@ignore v
    '(declare (ignore v))
    "ignore")
(is '@ignorable v
    '(declare (ignorable v))
    "ignorable")
(is '@type (integer v)
    '(declare (type (integer v)))
    "type")
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
