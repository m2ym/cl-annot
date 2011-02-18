(in-package :cl-user)

(defpackage cl-annot-test
  (:use :cl
        :cl-test-more
        :annot.eval-when
        :annot.doc))

(in-package :cl-annot-test)

(annot:enable-annot-syntax)

(defun symbol-status (name)
  (cadr (multiple-value-list (find-symbol (string-upcase name)))))

(is @1+ 1
    2
    "expression")
(is-expand @1+ 1
           (1+ 1)
           "expression expansion")
(is @or 1
    1
    "macro")
(is-expand @or 1
           (or 1)
           "macro expansion")
(is @export (defun x ())
    'x
    "export function")
(is (symbol-status :x)
    :external
    "function exported?")
(is @export (defun (setf s) ())
    '(setf s)
    "export setf function")
(is (symbol-status :s)
    :external
    "setf function exported?")
(is-type @export (defgeneric g ())
         'standard-generic-function
         "export generic function")
(is (symbol-status :g)
    :external
    "generic function exported?")
(is-type @export (defmethod m ())
         'standard-method
         "export method")
(is (symbol-status :m)
    :external
    "method exported?")
(is-type @export (defclass c () ())
         'standard-class
         "export class")
(is (symbol-status :c)
    :external
    "class exported?")
(is (macroexpand '@export (defun x ()))
    '(progn
      (cl-annot.std:export*
       (defun x ()))
      (defun x ()))
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
(is-expand @eval-when-compile 1
           (eval-when-compile 1)
           "eval-when-compile")
(is-expand @eval-when-load 1
           (eval-when-load 1)
           "eval-when-load")
(is-expand @eval-when-execute 1
           (eval-when-execute 1)
           "eval-when-execute")
(is-expand @eval-always 1
           (eval-always 1)
           "eval-always")
(is-expand @doc "doc" (defun f () 1)
           (doc "doc" (defun f () 1))
           "function documentation expansion")
(is @doc "doc" (defparameter p nil)
    'p
    "parameter documentation")
(is (documentation 'p 'variable)
    "doc"
    "parameter documented?")
(is @doc "doc" (defconstant k nil)
    'k
    "constant documentation")
(is (documentation 'k 'variable)
    "doc"
    "constant documented?")
(is @doc "doc" (defun f () 1)
    'f
    "function documentation")
(is (documentation 'f 'function)
    "doc"
    "function documented?")
(is @doc "doc" (defmacro mac () 1)
    'mac
    "macro documentation")
(is (documentation 'mac 'function)
    "doc"
    "macro documented?")
(is @export @doc "doc" (defun x () 1)
    'x
    "export&doc")
(is (symbol-status :x)
    :external
    "export&doc exported?")
(is (documentation 'x 'function)
    "doc"
    "export&doc documented?")

(finalize)
