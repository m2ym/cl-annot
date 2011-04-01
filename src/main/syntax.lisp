(defpackage cl-annot.syntax
  (:nicknames :annot.syntax)
  (:use :cl
        :annot.core
        :annot.expand)
  (:export :annotation-syntax-reader))
(in-package :annot.syntax)

(defun read-annotation (stream)
  (let ((annot (read stream t nil t)))
    (or (annotation-real annot) annot)))

(defun read-annotation-arguments (stream arity)
  (loop repeat arity collect (read stream t nil t)))

(defun annotation-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annot (read-annotation stream))
         (arity (annotation-arity annot))
         (args (read-annotation-arguments stream arity)))
    (if (annotation-inline-p annot)
        (expand-annotation annot args)
        (annotation-form annot args))))
