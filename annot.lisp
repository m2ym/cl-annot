(in-package :cl-user)

(defpackage cl-annot
  (:use :cl)
  (:nicknames :annot)
  (:export :function-name
           :method-function
           :method-name
           :reference-symbol
           :enable-annot-syntax))

(in-package :cl-annot)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collect `(,var ',(gensym)))
     ,@body))

(defun function-name (function)
  "Return the symbol of FUNCTION."
  #+sbcl (slot-value function 'sb-pcl::name)
  #+cmu (slot-value function 'pcl::name)
  #+ccl (slot-value function 'ccl::name)
  #+allegro (slot-value function 'excl::name)
  #+clisp (slot-value function 'clos::$name)
  #+(or ecl lispworks) (slot-value function 'clos::name)
  #-(or sbcl cmu ccl allegro clisp ecl lispworks)
  (error "FUNCTION-NAME is not supported"))

(defun method-function (method)
  "Return the generic function of METHOD."
  #+sbcl (slot-value method 'sb-pcl::%generic-function)
  #+(or cmu ccl) (slot-value method 'generic-function)
  #+allegro (slot-value method 'generic-function)
  #+clisp (slot-value method 'clos::$gf)
  #+(or ecl lispworks) (slot-value method 'generic-function)
  #-(or sbcl cmu ccl allegro clisp ecl lispworks)
  (error "METHOD-FUNCTION is not supported"))

(defun method-name (method)
  "Return the symbol of METHOD."
  (function-name (method-function method)))

(defun reference-symbol (object)
  "Return the reference symbol of OBJECT."
  (etypecase object
    (cons
     (if (eq (car object) 'cl:setf)
         (cadr object)
         (error "No cons reference symbol")))
    (symbol object)
    (class (class-name object))
    (standard-generic-function (function-name object))
    (standard-method (method-name object))))

(defun macrop (object)
  "Return non-nil if OBJECT is a macro."
  (and (symbolp object)
       (macro-function object)
       t))

(defun macroexpand-some (form)
  "Expand FORM while it has a valid form."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p)
            (null new-form))
        form
        (macroexpand-some new-form))))

(defun export* (object)
  "Export the reference symbol of OBJECT."
  (export (reference-symbol object)))

(defmacro ignore* (vars)
  "Shorthand of (DECLARE (IGNORE ...))."
  (if (listp vars)
      `(declare (ignore ,@vars))
      `(declare (ignore ,vars))))

(defmacro type* (type-specs)
  "Shothand of (DECLARE (TYPE ...))."
  `(declare (type ,type-specs)))

(defun read-annotation (stream)
  "Read an annotation from STREAM."
  (let ((annot (read stream t nil t)))
    (case annot
      (cl:export 'export*)
      (cl:ignore (values 'ignore* t))
      (cl:type (values 'type* t))
      (t annot))))

(defun annot-syntax-reader (stream char)
  (declare (ignore char))
  (multiple-value-bind (annot expand-p)
      (read-annotation stream)
    (let* ((arg (read stream t nil t))
           (form `(,annot ,arg)))
      (if (macrop annot)
          (if expand-p
              (macroexpand-some form)
              form)
          (with-gensyms (v)
            `(let ((,v ,arg))
               (,annot ,v) ,v))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enable-annot-syntax ()
    (set-macro-character #\@ #'annot-syntax-reader)))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))