(in-package :cl-user)

(defpackage cl-annot-test
  (:use :cl
        :cl-test-more
        :annot.eval-when
        :annot.doc
        :annot.class
        :annot.slot))

(in-package :cl-annot-test)

(annot:enable-annot-syntax)

(defun symbol-status (name)
  (cadr (multiple-value-list (find-symbol (string-upcase name)))))

(defmacro id-macro (x) x)

(defmacro fun () `(defun f ()))

(plan 65)

;;;; core

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

;;;; exporting

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
    "@export expansion 1")
(is (macroexpand '@export (fun))
    '(progn
      (export 'f)
      (fun))
    "@export expansion 2")
(is-type @export (defstruct s ())
         'symbol
         "export structure")
(is (symbol-status :s)
    :external
    "structure exported?")
(is-type @export (defstruct (s2 (:constructor make-s2)))
         'symbol
         "export structure with a long form")
(is (symbol-status :s2)
    :external
    "structure exported?")

;;;; ignore and other compilation-related declarations

(is '@ignore v
    '(declare (ignore v))
    "@ignore")
(is '@ignorable v
    '(declare (ignorable v))
    "@ignorable")
(is '@type integer v
    '(declare (type integer v))
    "@type")
(is '@optimize (speed 3)
    '(declare (optimize (speed 3)))
    "@optimize")
(is '@inline f
    '(declare (inline f))
    "@inline symbol")
(is '@inline (defun f ())
    '(progn
      (declaim (inline f))
      (defun f ()))
    "@inline defun")
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

;;;; doc

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
(is-type @doc "doc" (defmethod m () 1)
         'standard-method
         "@doc method")
(is (documentation (find-method (symbol-function 'm) nil ()) t)
    "doc"
    "method documented?")
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
(is-expand @metaclass persistent-class (defclass c () ())
           (defclass c () () (:metaclass persistent-class))
           "@metaclass expansion")
(is-expand @export-slots (defclass c () (a b c))
           (progn (export '(a b c)) (defclass c () (a b c)))
           "@export-slots expansion")
(is-expand @export-accessors
           (defclass c ()
                ((a :reader a-of)
                 (b :writer b-of)
                 (c :accessor c-of)))
           (progn
             (export '(a-of b-of c-of))
             (defclass c ()
                ((a :reader a-of)
                 (b :writer b-of)
                 (c :accessor c-of))))
           "@export-accessors expansion")
(is-expand @export-slots (defstruct s a b c)
           (progn (export '(a b c)) (defstruct s a b c))
           "@export-slots expansion for defstruct")
(is-expand @export-accessors
           (defstruct s a b c)
           (progn
             (export '(s-a s-b s-c))
             (defstruct s a b c))
           "@export-accessors expansion for defstruct")
(is-expand @export-accessors
           (defstruct (s (:conc-name abya)) a b c)
           (progn
             (export '(abyaa abyab abyac))
             (defstruct (s (:conc-name abya)) a b c))
           "@export-accessors expansion for defstruct")
(is-expand @export-accessors
           (defstruct (s (:conc-name)) a b c)
           (progn
             (export '(a b c))
             (defstruct (s (:conc-name)) a b c))
           "@export-accessors expansion for defstruct")
(is-expand @export-accessors
           (defstruct (s :conc-name) a b c)
           (progn
             (export '(a b c))
             (defstruct (s :conc-name) a b c))
           "@export-accessors expansion for defstruct")

;;;; constructor

(is-expand @export-constructors
           (defstruct s a b c)
           (progn
             (export '(make-s))
             (defstruct s a b c))
           "@export-constructors expansion for defstruct")
(is-expand @export-constructors
           (defstruct (s (:constructor abya)) a b c)
           (progn
             (export '(abya))
             (defstruct (s (:constructor abya)) a b c))
           "@export-constructors expansion for defstruct")
(is-expand @export-constructors
           (defstruct (s (:constructor abya a c)
                         (:constructor abya2 a b c)) a b c)
           (progn
             (export '(abya abya2))
             (defstruct (s (:constructor abya a c)
                           (:constructor abya2 a b c)) a b c))
           "@export-constructors expansion for defstruct")
(is-expand @export-constructors
           (defstruct (s (:constructor)) a b c)
           (progn
             (export '(make-s))
             (defstruct (s (:constructor)) a b c))
           "@export-constructors expansion for defstruct")
(is-expand @export-constructors
           (defstruct (s :constructor) a b c)
           (progn
             (export '(make-s))
             (defstruct (s :constructor)) a b c)
           "@export-constructors expansion for defstruct")
(is-expand @export-constructors
           (defstruct (s (:constructor nil)) a b c)
           (defstruct (s (:constructor nil)) a b c)
           "@export-constructors expansion for defstruct, no constructor")

(is '@required foo
    '(foo :initform (annot.slot::required-argument :foo) :initarg :foo)
    "@required expansion 1")
(is '@required (foo :initarg :bar)
    '(foo :initform (annot.slot::required-argument :bar) :initarg :bar)
    "@required expansion 2")
(is '@optional nil foo
    '(foo :initform nil :initarg :foo)
    "@optional expansion 1")
(is '@optional nil (foo :initarg :bar)
    '(foo :initform nil :initarg :bar)
    "@optional expansion 2")

(finalize)
