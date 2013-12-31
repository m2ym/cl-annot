;;; slime-annot.el --- cl-annot support for SLIME

;; URL: https://github.com/arielnetworks/cl-annot
;; Package-Requires: ((slime "0"))

;;; Commentary:

;;; Code:

(require 'cl)
(require 'slime)

(defvar slime-annotation-face 'highlight)
(font-lock-add-keywords 'lisp-mode `(("\\(?:^\\|[^,]\\)\\(@\\(?:\\sw\\|\\s_\\)+\\)" (1 ,slime-annotation-face))))

(defvar slime-annotation-max-arity 3)

(defun slime-beginning-of-annotation ()
  (interactive)
  (ignore-errors
    (let ((point (point)) found)
      (save-excursion
        (setq found
              (loop repeat slime-annotation-max-arity
                    while 
                    (save-excursion
                      (let ((point (point)))
                        (backward-sexp)
                        (forward-sexp)
                        (<= (count-lines (point) point) 1)))
                    do (backward-sexp)
                    if (eq (char-after) ?\@)
                    return (point))))
      (when found
        (goto-char found)))))

(defun slime-beginning-of-annotation* ()
  (interactive)
  (while (slime-beginning-of-annotation)))

(defadvice slime-region-for-defun-at-point (after slime-region-for-defun-at-point-with-annotations activate)
  (save-excursion
    (goto-char (car ad-return-value))
    (slime-beginning-of-annotation*)
    (setq ad-return-value (list (point) (cadr ad-return-value)))))

(provide 'slime-annot)

;;; slime-annot.el ends here
