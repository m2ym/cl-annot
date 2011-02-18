(in-package :cl-user)

(defpackage cl-annot.expand
  (:use :cl
        :annot.core)
  (:nicknames :annot.expand)
  (:export :expand-annotation
           :annotation))

(in-package :annot.expand)

(defun toplevel-annotation-form-p (form)
  (and (consp form)
       (consp (cdr form))
       (eq (car form) 'annotation)
       (toplevel-annotation-p (cadr form))))

(defun expand-normal-annotation (annot args)
  `(,annot ,@args))

(defun expand-toplevel-annotation (annot args)
    (loop with body = `(annotation ,annot ,@args)
          while (toplevel-annotation-form-p body)
          collect body into annots
          do (setf body (car (last body)))
          finally
       (return
         `(progn
            ,@(loop for annot in annots
                    collect `(,@(cdr (butlast annot)) ,body))
            ,body))))

(defun expand-annotation (annot args)
  (if (toplevel-annotation-p annot)
      (expand-toplevel-annotation annot args)
      (expand-normal-annotation annot args)))

(defmacro annotation (annot &rest args)
  "Annotation Expansion Engine."
  (expand-annotation annot args))
