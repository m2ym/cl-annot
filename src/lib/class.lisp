(in-package :cl-user)
(defpackage cl-annot.class
  (:nicknames :annot.class)
  (:use :cl
        :annot.util
        :annot.helper)
  (:export :metaclass
           :export-slots
           :export-accessors
           :export-constructors
           :export-structure
           :export-class)
  (:import-from :alexandria
                :ensure-list
                :curry
                :compose
                :if-let
                :symbolicate))
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
  (let ((options (ensure-list (second class-definition-form))))
    (if-let ((conc-name
              (find-if (lambda (option)
                         (and (consp option) (eq (first option) :conc-name)))
                       options)))
      (second conc-name)
      (if (find :conc-name options)
          nil
          (symbolicate (first options) '-)))))

(defun get-accessors-in-defstruct (class-definition-form)
  `(progn
     (export ',(mapcar (compose
                        (let ((conc-name (get-conc-name class-definition-form)))
                          (if conc-name
                              (curry #'symbolicate conc-name)
                              #'identity))
                        #'first
                        #'ensure-list)
                       (slot-specifiers class-definition-form)))
     ,class-definition-form))

(defmacro export-constructors (class-definition-form)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (case (first class-definition-form)
       (defstruct
           (if (consp (second class-definition-form))
               (let ((constructor-clauses
                      (remove-if-not
                       (lambda (lst) (eq (first lst) :constructor))
                       (mapcar #'ensure-list
                               (cdr (second class-definition-form))))))
                 (if (and (= 1 (length constructor-clauses))
                          (= 2 (length (car constructor-clauses)))
                          (null (cadar constructor-clauses)))
                     class-definition-form
                     `(progn
                        (export
                         ',(or (remove nil (mapcar #'second constructor-clauses))
                               (list (symbolicate
                                      'make- (first (second class-definition-form))))))
                        ,class-definition-form)))
               `(progn
                  (export
                   '(,(symbolicate
                       'make- (second class-definition-form))))
                  ,class-definition-form)))
       (t class-definition-form)))
   class-definition-form))

(defmacro export-class (class-definition-form)
  `(annot.std:export*
    (export-slots
     (export-accessors
      ,class-definition-form))))

(defmacro export-structure (class-definition-form)
  `(annot.std:export*
    (export-slots
     (export-accessors
      (export-constructors
       ,class-definition-form)))))
