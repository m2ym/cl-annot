(in-package :cl-user)

(defpackage cl-annot.util
  (:use :cl)
  (:nicknames :annot.util)
  (:export :macrop
           :macroexpand-some
           :definition-symbol
           :progn-last
           :progn-replace-last
           :definition-type
           :with-gensyms))

(in-package :cl-annot.util)

(defun macrop (object)
  "Return non-nil if OBJECT is a macro."
  (and (symbolp object)
       (macro-function object)
       t))

(defun macroexpand-some (form)
  "Expand FORM once."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p) (null new-form))
        form
        new-form)))

(defun progn-last (progn)
  "Return the last form of PROGN which should evaluted at last."
  (if (and (consp progn)
           (eq (car progn) 'progn))
      (progn-last (car (last progn)))
      progn))

(defun progn-replace-last (last progn)
  "Replace the last form of PROGN with LAST."
  (if (and (consp progn)
           (eq (car progn) 'progn))
      `(,@(butlast progn)
          ,(progn-replace-last last (car (last progn))))
      last))

(defun definition-symbol (definition-form)
  "Return the symbol of DEFINITION-FORM."
  ;; TODO syntax check
  (cadr definition-form))

(defun definition-type (definition-form)
  "Return the type of DEFINITION-FORM."
  ;; TODO macroexpand
  (car definition-form))

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collect `(,var ',(gensym)))
     ,@body))
