;;; edit-symbol.el --- Edit the symbol at point. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jens C. Jensen

;; Author: Jens C. Jensen <jens@subst.net>
;; URL:
;; Keywords:
;; Package-Requires ((emacs "28.0.50"))
;; Package-Version: 20210402
;; Version: 0.1.0


;;; Commentary:
;;

;;; Code:

;;;; core

(defvar-local edit-symbol--symbol nil)
(defvar-local edit-symbol--original nil)

(defun edit-symbol--proper-p (sym)
  ;; (or
  ;;   (null sym)

  ;;   (floatp sym)
  ;;   (bignump sym)
  ;;   (fixnump sym)

  ;;   (consp sym)
  ;;   (listp sym)
  ;;   (arrayp sym)
  ;;   (vectorp sym)

  ;;   (stringp sym)
  ;;   (booleanp sym)
  ;;   (integerp sym))
  (and
   (symbolp sym)
   (boundp sym)
   ))

(defun edit-symbol-at-point ()
  "Edit symbol at point in an elisp buffer."
  (interactive)
  (let ((sym (symbol-at-point)))
    (unless (edit-symbol--proper-p sym)
      (user-error "%s if not a proper symbol" sym))

    (let* ((v (symbol-value sym))
           (n (symbol-name sym))
           (b-name (format "*edit-symbol: %s*" n))
           (b (generate-new-buffer b-name)))

      (with-current-buffer b
        (pp v (current-buffer))
        (goto-char (point-min))
        (edit-symbol-mode))

      (setf (buffer-local-value 'edit-symbol--symbol b) sym)
      (setf (buffer-local-value 'edit-symbol--original b) v)
      (switch-to-buffer-other-window b 'norecord))))

(defun edit-symbol--done ()
  (interactive)
  (unless (derived-mode-p 'edit-symbol-mode)
    (error "This is not an edit-symbol buffer"))

  (unless (and (symbolp edit-symbol--symbol)
               (boundp edit-symbol--symbol))
    (user-error "%s is not a bound symbol" edit-symbol--symbol))

  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (set edit-symbol--symbol (read (current-buffer)))
        (message "edit-symbol: set %s" edit-symbol--symbol))
    (error
     (set edit-symbol--symbol edit-symbol--original)
     (message "edit-symbol: restored %s" edit-symbol--symbol))))

(defun edit-symbol--quit ()
  (interactive)
  (unless (derived-mode-p 'edit-symbol-mode)
    (error "This is not an edit-symbol buffer"))

  (quit-window 'kill))

(defun edit-symbol--done-quit ()
  (interactive)
  (edit-symbol--done)
  (edit-symbol--quit))

;;;; mode

(defvar edit-symbol-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-x C-s") 'edit-symbol--done)
    (define-key map (kbd "C-c C-k") 'edit-symbol--quit)
    (define-key map (kbd "C-c C-q") 'edit-symbol--quit)
	  (define-key map (kbd "C-c C-c") 'edit-symbol--done-quit)
	  map))

(define-derived-mode edit-symbol-mode emacs-lisp-mode "edit-sym"
	"Major mode to edit a symbol.")

;;;; epilogue


(provide 'edit-symbol)
;;; edit-symbol.el ends here
