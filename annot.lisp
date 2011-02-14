(in-package :cl-user)

(defpackage cl-annot
  (:use :cl)
  (:nicknames :annot)
  (:export :enable-annot-syntax))

(in-package :cl-annot)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars
               collect `(,var ',(gensym)))
     ,@body))

(defparameter *annot-nest* 0)

(defun annot-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annotator (read stream t nil t))
         (*annot-nest* (1+ *annot-nest*))
         (arg (read stream t nil t)))
    (if (eq *annot-nest* 0)
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
