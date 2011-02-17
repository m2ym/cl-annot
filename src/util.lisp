(in-package :cl-user)

(defpackage cl-annot.util
  (:use :cl)
  (:nicknames :annot.util)
  (:export :with-gensyms
           :macrop
           :macroexpand-some
           :should-expand-p
           :annotation))

(in-package :cl-annot.util)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collect `(,var ',(gensym)))
     ,@body))

(defun macrop (object)
  "Return non-nil if OBJECT is a macro."
  (and (symbolp object)
       (macro-function object)
       t))

(defun macroexpand-some (form)
  "Expand FORM while it has a normal form."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p)
            (null new-form))
        form
        (macroexpand-some new-form))))

(defun should-expand-p (annot)
  "Return non-nil if ANNOT should be expanded on read-time."
  (and (symbolp annot)
       (get annot 'should-expand-p)))

(defun (setf should-expand-p) (expand-p annot)
  (setf (get annot 'should-expand-p) expand-p))

(defun annotation (annot)
  "Return the real annotation of ANNOT."
  (and (symbolp annot)
       (get annot 'annotation)))

(defun (setf annotation) (real annot)
  (setf (get annot 'annotation) real))
