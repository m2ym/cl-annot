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
   class-definition-form))

(defmacro export-class (class-definition-form)
  `(annot.std:export*
    (export-slots
     (export-accessors
      ,class-definition-form))))
