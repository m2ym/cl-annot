(in-package :cl-user)

(defpackage cl-annot
  (:use :cl
        :cl-annot.util
        :cl-annot.std)
  (:nicknames :annot)
  (:export :enable-annot-syntax))

(in-package :cl-annot)

(defun read-annotation (stream)
  "Read an annotation from STREAM."
  (let ((annot (read stream t nil t)))
    (or (annotation-macro annot) annot)))

(defun annot-syntax-reader (stream char)
  (declare (ignore char))
  (let* ((annot (read-annotation stream))
         (arg (read stream t nil t))
         (form `(,annot ,arg)))
    (if (macrop annot)
        (if (should-expand-p annot)
            (macroexpand-some form)
            form)
        (with-gensyms (v)
          `(let ((,v ,arg))
             (,annot ,v) ,v)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enable-annot-syntax ()
    (set-macro-character #\@ #'annot-syntax-reader)))

(defmacro enable-annot-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-annot-syntax)))