(in-package :cl-user)

(defpackage cl-annot-test
  (:use :cl
        :cl-test-more
        :annot.eval-when
        :annot.doc
        :annot.slot))

(in-package :cl-annot-test)

(annot:enable-annot-syntax)

(defun symbol-status (name)
  (cadr (multiple-value-list (find-symbol (string-upcase name)))))

(defmacro id-macro (x) x)

(is @1+ 1
    2
    "expression")
(is-expand @1+ 1
           (1+ 1)
           "expression expansion")
(is @id-macro 1
    1
    "macro")
(is-expand @id-macro 1
           1
           "macro expansion")
(is @export (defun x ())
    'x
    "@export function")
(is (symbol-status :x)
    :external
    "function exported?")
(is @export (defun (setf s) ())
    '(setf s)
    "@export setf function")
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
      (export 'x)
      (defun x ()))
    "@export expansion")
(is '@ignore v
    '(declare (ignore v))
    "@ignore")
(is '@ignorable v
    '(declare (ignorable v))
    "@ignorable")
(is '@type (integer v)
    '(declare (type integer v))
    "@type")
(is-expand @eval-when-compile 1
           (eval-when (:compile-toplevel) 1)
           "@eval-when-compile")
(is-expand @eval-when-load 1
           (eval-when (:load-toplevel) 1)
           "@eval-when-load")
(is-expand @eval-when-execute 1
           (eval-when (:execute) 1)
           "@eval-when-execute")
(is-expand @eval-always 1
           (eval-when (:compile-toplevel
                       :load-toplevel
                       :execute) 1)
           "@eval-always")
(is-expand @doc "doc" (defun f () 1)
           (defun f () "doc" 1)
           "function documentation expansion")
(is @doc "doc" (defparameter p nil)
    'p
    "@doc parameter")
(is (documentation 'p 'variable)
    "doc"
    "parameter documented?")
(is @doc "doc" (defconstant k nil)
    'k
    "@doc constant")
(is (documentation 'k 'variable)
    "doc"
    "constant documented?")
(is @doc "doc" (defun f () 1)
    'f
    "@doc function")
(is (documentation 'f 'function)
    "doc"
    "function documented?")
(let ((m @doc "doc" (defmethod m () 1)))
  (is-type m
           'standard-method
           "@doc method")
  (is (documentation m t)
      "doc"
      "method documented?"))
(is @doc "doc" (defmacro mac () 1)
    'mac
    "@doc macro")
(is (documentation 'mac 'function)
    "doc"
    "macro documented?")
(is @export @doc "doc" (defun y () 1)
    'y
    "@export and @doc")
(is (symbol-status :y)
    :external
    "@export and @doc exported?")
(is (documentation 'y 'function)
    "doc"
    "@export and @doc documented?")
(is @doc "doc" @export (defun z () 1)
    'z
    "@doc and @export")
(is (symbol-status :z)
    :external
    "@doc and @export exported?")
(is (documentation 'z 'function)
    "doc"
    "@doc and @export documented?")
(is '@initarg (foo)
    '(foo :initarg :foo)
    "@initarg expansion")
(is '@required (foo :initarg :foo)
    '(foo :initform (error "Must supply :FOO") :initarg :foo)
    "@required expansion")

(finalize)
