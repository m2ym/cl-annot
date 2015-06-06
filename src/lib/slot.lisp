(in-package :cl-user)
(defpackage cl-annot.slot
  (:nicknames :annot.slot)
  (:use :cl
        :annot.util
        :annot.helper)
  (:import-from :alexandria
                :make-keyword
                :with-gensyms)
  (:export :optional
           :required))
(in-package :annot.slot)

(defun required-argument (name)
  (error "Must supply ~S" name))

(defmacro def-slot-annotation (name args &body body)
  (with-gensyms (slot-specifier)
    `(defannotation ,name ,(append args (list slot-specifier))
         (:inline t :arity ,(1+ (length args)))
       (destructuring-bind (slot-name . slot-options)
           (if (consp ,slot-specifier)
               ,slot-specifier
               (list ,slot-specifier))
         ,@body
         (cons slot-name slot-options)))))

(def-slot-annotation optional (init-form)
  (unless (plist-member slot-options :initarg)
    (setf (getf slot-options :initarg)
          (make-keyword slot-name)))
  (unless (plist-member slot-options :initform)
    (setf (getf slot-options :initform) init-form)))

(def-slot-annotation required ()
  (when (plist-member slot-options :initform)
    (error "Required slot ~A must not have :initform" slot-name))
  (unless (plist-member slot-options :initarg)
    (setf (getf slot-options :initarg)
          (make-keyword slot-name)))
  (setf (getf slot-options :initform)
        `(required-argument ,(getf slot-options :initarg))))
