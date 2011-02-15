(in-package :cl-user)

(defpackage cl-annot
  (:use :cl)
  (:nicknames :annot)
  (:export :function-name
           :method-function
           :method-name
           :value-symbol
           :enable-annot-syntax))

(in-package :cl-annot)

(defparameter *annot-nest* 0)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars
               collect `(,var ',(gensym)))
     ,@body))

(defun function-name (function)
  "Return the symbol of the FUNCTOIN."
  #+sbcl (slot-value function 'sb-pcl::name)
  #+ccl (slot-value function 'ccl::name)
  #+allegro (slot-value function 'excl::name)
  #+clisp (slot-value function 'clos::$name)
  #+(or ecl lispworks) (slot-value function 'clos::name)
  #-(or sbcl ccl allegro clisp ecl lispworks) (error "method-name is not supported"))

(defun method-function (method)
  "Return the generic function of the METHOD."
  #+sbcl (slot-value method 'sb-pcl::%generic-function)
  #+ccl (slot-value method 'generic-function)
  #+allegro (slot-value method 'generic-function)
  #+clisp (slot-value method 'clos::$gf)
  #+(or ecl lispworks) (slot-value method 'generic-function)
  #-(or sbcl ccl allegro clisp ecl lispworks) (error "method-function is not supported"))

(defun method-name (method)
  "Return the symbol of the METHOD."
  (function-name (method-function method)))

(defun value-symbol (value)
  "Return the symbol of the VALUE. VALUE can be symbol, class, and
method."
  (etypecase value
    (cons
     (if (eq (car value) 'cl:setf)
         (cadr value)
         (error "Unknown cons format")))
    (symbol value)
    (class (class-name value))
    (standard-generic-function (function-name value))
    (standard-method (method-name value))))

(defun export* (value)
  (export (value-symbol value)))

(defmacro ignore* (var)
  `(declare (ignore ,var)))

(defun read-annotator (stream)
  "Read annotator specification from the STREAM."
  (let ((annotator (read stream t nil t)))
    (case annotator
      (cl:export 'export*)
      (cl:ignore (values 'ignore* t))
      (t annotator))))

(defun annot-syntax-reader (stream char)
  (declare (ignore char))
  (multiple-value-bind (annotator macroexpand)
      (read-annotator stream)
    (let* ((*annot-nest* (1+ *annot-nest*))
           (arg (read stream t nil t)))
      (if (eq *annot-nest* 1)
          (let ((form `(,annotator ,arg)))
            (if macroexpand
                (macroexpand form)
                form))
          (with-gensyms (v)
            `(let ((,v ,arg))
               (,annotator ,v) ,v))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enable-annot-syntax ()
    (set-macro-character #\@ #'annot-syntax-reader)))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
