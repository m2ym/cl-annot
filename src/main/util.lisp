(defpackage cl-annot.util
  (:use :cl)
  (:nicknames :annot.util)
  (:export :macrop
           :macroexpand-some
           :progn-form-last
           :progn-form-replace-last
           :definition-form-symbol
           :definition-form-type
           :map-slot-specifiers))

(in-package :annot.util)

(defun macrop (symbol)
  "Return non-nil if SYMBOL is a macro."
  (and (symbolp symbol)
       (macro-function symbol)
       t))

(defun macroexpand-some (form)
  "Expand FORM once. The result form won't be nil."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p) (null new-form))
        (values form nil)
        (values new-form expanded-p))))

(defun progn-form-last (progn-form)
  "Return the last form of PROGN-FORM which should evaluated at last."
  (if (and (consp progn-form)
           (eq (car progn-form) 'progn))
      (progn-form-last (car (last progn-form)))
      progn-form))

(defun progn-form-replace-last (last progn-form)
  "Replace the last form of PROGN-FORM with LAST. If LAST is a
function, the function will be called with the last form and used for
replacing."
  (if (and (consp progn-form)
           (eq (car progn-form) 'progn))
      `(,@(butlast progn-form)
          ,(progn-form-replace-last last (car (last progn-form))))
      (if (functionp last)
          (funcall last progn-form)
          last)))

(defun definition-form-symbol (definition-form)
  "Return the symbol of DEFINITION-FORM."
  ;; TODO
  (cadr definition-form))

(defun definition-form-type (definition-form)
  "Return the type of DEFINITION-FORM."
  ;; TODO
  (car definition-form))

(defun map-slot-specifiers (function class-definition-form)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (destructuring-bind (type name supers slots &rest options)
         class-definition-form
       `(,type ,name ,supers ,(mapcar function slots) ,@options)))
   class-definition-form))
