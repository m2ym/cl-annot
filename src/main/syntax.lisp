(in-package :cl-user)

(defpackage cl-annot.syntax
  (:use :cl
        :macro-utils
        :annot.core
        :annot.expand)
  (:nicknames :annot.syntax)
  (:export :enable-annot-syntax))

(in-package :annot.syntax)

(defun read-annotation (stream)
  (let ((annot (read stream t nil t)))
    (or (annotation-real annot) annot)))

(defun read-annotation-arguments (stream narg)
  (loop repeat narg collect (read stream t nil t)))

(defun annotation-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annot (read-annotation stream))
         (narg (annotation-narg annot))
         (args (read-annotation-arguments stream narg)))
    (if (annotation-inline-p annot)
        (expand-annotation annot args)
        (annotation-form annot args))))

(defun %enable-annot-syntax ()
  (set-macro-character #\@ #'annotation-syntax-reader))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))
