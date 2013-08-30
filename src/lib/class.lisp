(defpackage cl-annot.class
  (:nicknames :annot.class)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :metaclass
           :export-slots
           :export-accessors
           :export-class))
(in-package :annot.class)

(defannotation metaclass (metaclass class-definition-form)
    (:arity 2)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (if (get-class-option :metaclass class-definition-form)
        (error ":metaclass is not empty")
        (append class-definition-form
                `((:metaclass ,metaclass)))))
   class-definition-form))

(defmacro export-slots (class-definition-form)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (loop for slot-specifier in (slot-specifiers class-definition-form)
           for slot = (if (consp slot-specifier)
                          (car slot-specifier)
                          slot-specifier)
           collect slot into slots
           finally
        (return
          (if slots
              `(progn
                 (export ',slots)
                 ,class-definition-form)
              class-definition-form))))
   class-definition-form))

(defmacro export-accessors (class-definition-form)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (case (first class-definition-form)
       (defclass (get-accessors-in-defclass class-definition-form))
       (defstruct (get-accessors-in-defstruct class-definition-form))))
   class-definition-form))

(defun get-accessors-in-defclass (class-definition-form)
  (loop for slot-specifier in (slot-specifiers class-definition-form)
     for slot-options = (when (consp slot-specifier) (cdr slot-specifier))
     if slot-options
     append (plist-get-all slot-options :reader) into accessors
     and append (plist-get-all slot-options :writer) into accessors
     and append (plist-get-all slot-options :accessor) into accessors
     finally
       (return
	 (if accessors
	     `(progn
		(export ',accessors)
		,class-definition-form)
	     class-definition-form))))

(defun get-conc-name (class-definition-form)
  (if (consp (second class-definition-form))
      (alexandria:if-let ((conc-name
			   (find-if (lambda (option) 
				      (and (consp option) (eq (first option) :conc-name)))
				    (second class-definition-form))))
	(second conc-name)
	(if (find :conc-name (second class-definition-form))
	    nil
	    (concatenate-symbols (first (second class-definition-form)) '-)))
      (concatenate-symbols (second class-definition-form) '-)))

(defun concatenate-symbols (a b)
  (intern (format nil "~a~a" a b)))

(defun get-accessors-in-defstruct (class-definition-form)
  `(progn
     (export ',(mapcar (alexandria:compose
			(let ((conc-name (get-conc-name class-definition-form)))
			  (if conc-name
			      (alexandria:curry #'concatenate-symbols conc-name)
			      #'identity))
			#'first
			#'alexandria:ensure-list)
		       (slot-specifiers class-definition-form)))
     ,class-definition-form))

(defmacro export-class (class-definition-form)
  `(annot.std:export*
    (export-slots
     (export-accessors
      ,class-definition-form))))
