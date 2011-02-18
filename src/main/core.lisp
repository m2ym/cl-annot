(in-package :cl-user)

(defpackage cl-annot.core
  (:use :cl)
  (:nicknames annot.core)
  (:export :real-annotation
           :annotation-narg
           :toplevel-annotation-p
           :annotation-expand-p))

(in-package :annot.core)

(defun real-annotation (annot)
  "Return the real annotation of ANNOT."
  (get annot 'real-annotation))

(defun (setf real-annotation) (real-annot annot)
  (setf (get annot 'real-annotation) real-annot))

(defun annotation-narg (annot)
  "Return the number of arguments of ANNOT."
  (or (get annot 'annotation-narg) 1))

(defun (setf annotation-narg) (narg annot)
  (setf (get annot 'annotation-narg) narg))

(defun toplevel-annotation-p (annot)
  "Return non-nil if ANNOT respects of top-level forms."
  (get annot 'toplevel-annotation-p))

(defun (setf toplevel-annotation-p) (toplevel-p annot)
  (setf (get annot 'toplevel-annotation-p) toplevel-p))

(defun annotation-expand-p (annot)
  "Return non-nil if ANNOT should expand at read-time."
  (get annot 'annotation-expand-p))

(defun (setf annotation-expand-p) (expand-p annot)
  (setf (get annot 'annotation-expand-p) expand-p))
