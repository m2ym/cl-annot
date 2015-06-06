(in-package :cl-user)
(defpackage cl-annot.expand
  (:nicknames :annot.expand)
  (:use :cl
        :annot.util
        :annot.core)
  (:export :expand-annotation))
(in-package :annot.expand)

(defun expand-annotation (annot args)
  "Expand ANNOT. ARGS will be expanded prior to this
form (call-by-value)."
  (let ((args (mapcar #'expand-annotation-form args)))
    (values (macroexpand-some `(,annot ,@args)))))

(defun expand-annotation-form (form)
  "Expand annotation FORM if possible."
  (if (annotation-form-p form)
      (expand-annotation (cadr form) (cddr form))
      form))

(defmacro %annotation (annot &rest args)
  "Annotation Expansion Engine."
  (expand-annotation annot args))
