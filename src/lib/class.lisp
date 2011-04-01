(defpackage cl-annot.class
  (:nicknames :annot.class)
  (:use :cl
        :annot.util
        :annot.api))
(in-package :annot.class)
(annot:enable-annot-syntax)

@export
@annotation (:arity 2)
(defmacro metaclass (metaclass class-definition-form)
  (if (get-class-option :metaclass class-definition-form)
      (error ":metaclass is not empty")
      (append class-definition-form
              `((:metaclass ,metaclass)))))

@export
(defmacro export-slots (class-definition-form)
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

@export
(defmacro export-accessors (class-definition-form)
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
