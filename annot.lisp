(in-package :cl-user)

(defpackage cl-annot
  (:use :cl)
  (:nicknames :annot)
  (:export :method-name
           :value-symbol
           :export*
           :enable-annot-syntax))

(in-package :cl-annot)

(defparameter *annot-nest* 0)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars
               collect `(,var ',(gensym)))
     ,@body))

(defun method-name (method)
  "Return the symbol of the METHOD."
  #+sbcl (slot-value (slot-value method 'sb-pcl::%generic-function) 'sb-pcl::name)
  #+ccl (slot-value (slot-value method 'generic-function) 'ccl::name)
  #+allegro (slot-value (slot-value method 'generic-function) 'excl::name)
  #+(or ecl lispworks) (slot-value (slot-value method 'generic-function) 'clos::name)
  #-(or sbcl ccl allegro ecl lispworks) (error "method-name is not supported"))

(defun value-symbol (value)
  "Return the symbol of the VALUE. VALUE can be symbol, class, and
method."
  (etypecase value
    (symbol value)
    (class (class-name value))
    (standard-method (method-name value))))

(defun export* (value)
  "Export the symbol of the VALUE. Unlike EXPORT, VALUE could also be
classes and methods."
  (export (value-symbol value)))

(defun read-annotator (stream)
  "Read annotator specification from the STREAM."
  (let ((annotator (read stream t nil t)))
    (case annotator
      (cl:export 'export*)
      (t annotator))))

(defun annot-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((*annot-nest* (1+ *annot-nest*))
         (annotator (read-annotator stream))
         (arg (read stream t nil t)))
    (if (eq *annot-nest* 1)
        `(,annotator ,arg)
        (with-gensyms (v)
          `(let ((,v ,arg))
             (,annotator ,v) ,v)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enable-annot-syntax ()
    (set-macro-character #\@ #'annot-syntax-reader)))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
