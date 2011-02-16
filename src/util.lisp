(in-package :cl-user)

(defpackage cl-annot.util
  (:use :cl)
  (:nicknames :annot.util)
  (:export :with-gensyms
           :function-name
           :method-function
           :method-name
           :reference-symbol
           :macrop
           :macroexpand-some
           :should-expand-p
           :annotation-macro))

(in-package :cl-annot.util)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collect `(,var ',(gensym)))
     ,@body))

(defun function-name (function)
  "Return the symbol of FUNCTION."
  #+sbcl (slot-value function 'sb-pcl::name)
  #+cmu (slot-value function 'pcl::name)
  #+ccl (slot-value function 'ccl::name)
  #+allegro (slot-value function 'excl::name)
  #+clisp (slot-value function 'clos::$name)
  #+(or ecl lispworks) (slot-value function 'clos::name)
  #-(or sbcl cmu ccl allegro clisp ecl lispworks)
  (error "FUNCTION-NAME is not supported"))

(defun method-function (method)
  "Return the generic function of METHOD."
  #+sbcl (slot-value method 'sb-pcl::%generic-function)
  #+(or cmu ccl) (slot-value method 'generic-function)
  #+allegro (slot-value method 'generic-function)
  #+clisp (slot-value method 'clos::$gf)
  #+(or ecl lispworks) (slot-value method 'generic-function)
  #-(or sbcl cmu ccl allegro clisp ecl lispworks)
  (error "METHOD-FUNCTION is not supported"))

(defun method-name (method)
  "Return the symbol of METHOD."
  (function-name (method-function method)))

(defun reference-symbol (object)
  "Return the reference symbol of OBJECT."
  (etypecase object
    (cons
     (if (eq (car object) 'cl:setf)
         (cadr object)
         (error "No cons reference symbol")))
    (symbol object)
    (class (class-name object))
    (standard-generic-function (function-name object))
    (standard-method (method-name object))))

(defun macrop (object)
  "Return non-nil if OBJECT is a macro."
  (and (symbolp object)
       (macro-function object)
       t))

(defun macroexpand-some (form)
  "Expand FORM while it has a valid form."
  (multiple-value-bind (new-form expanded-p)
      (macroexpand-1 form)
    (if (or (not expanded-p)
            (null new-form))
        form
        (macroexpand-some new-form))))

(defun should-expand-p (symbol)
  "Return non-nil if the macro of SYMBOL should be expaneded on
read-time."
  (and (symbolp symbol)
       (get symbol 'should-expand-p)))

(defun (setf should-expand-p) (bool symbol)
  (setf (get symbol 'should-expand-p) bool))

(defun annotation-macro (symbol)
  "Return the real annotation macro of SYMBOL."
  (and (symbolp symbol)
       (get symbol 'annotation-macro)))

(defun (setf annotation-macro) (macro symbol)
  (setf (get symbol 'annotation-macro) macro))
