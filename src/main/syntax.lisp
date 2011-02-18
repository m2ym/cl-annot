(in-package :cl-user)

(defpackage cl-annot.syntax
  (:use :cl
        :annot.core
        :annot.expand
        :annot.util)
  (:nicknames :annot.syntax)
  (:export :enable-annot-syntax))

(in-package :annot.syntax)

(defun read-annotation (stream)
  (let ((annot (read stream t nil t)))
    (or (real-annotation annot) annot)))

(defun read-annotation-arguments (stream narg)
  (loop repeat narg collect (read stream t nil t)))

(defun annotation-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annot (read-annotation stream))
         (narg (annotation-narg annot))
         (args (read-annotation-arguments stream narg))
         (form `(annotation ,annot ,@args)))
    (if (annotation-expand-p annot)
        (macroexpand-some form)
        form)))

(defun %enable-annot-syntax ()
  (set-macro-character #\@ #'annotation-syntax-reader))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
