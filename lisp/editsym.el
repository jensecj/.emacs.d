;;; editsym.el --- Some description. -*- lexical-binding: t; -*-

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

(defvar-local editsym--symbol nil)
(defvar-local editsym--original nil)

(defun editsym--proper-p (sym)
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

(defun editsym-at-point ()
  "Edit symbol at point in an elisp buffer."
  (interactive)
  (let ((sym (symbol-at-point)))
    (unless (editsym--proper-p sym)
      (user-error "%s if not a proper symbol" sym))

    (let* ((v (symbol-value sym))
           (n (symbol-name sym))
           (b-name (format "*editsym: %s*" n))
           (b (generate-new-buffer b-name)))

      (with-current-buffer b
        (pp v (current-buffer))
        (goto-char (point-min))
        (editsym-mode))

      (setf (buffer-local-value 'editsym--symbol b) sym)
      (setf (buffer-local-value 'editsym--original b) v)
      (switch-to-buffer-other-window b 'norecord))))

(defun editsym--done ()
  (interactive)
  (unless (derived-mode-p 'editsym-mode)
    (error "This is not an editsym buffer"))

  (unless (and (symbolp editsym--symbol)
               (boundp editsym--symbol))
    (user-error "%s is not a bound symbol" editsym--symbol))

  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (set editsym--symbol (read (current-buffer)))
        (message "editsym: set %s" editsym--symbol))
    (error
     (set editsym--symbol editsym--original)
     (message "editsym: restored %s" editsym--symbol))))

(defun editsym--quit ()
  (interactive)
  (unless (derived-mode-p 'editsym-mode)
    (error "This is not an editsym buffer"))

  (quit-window 'kill))

(defun editsym--done-quit ()
  (interactive)
  (editsym--done)
  (editsym--quit))

;;;; mode

(defvar editsym-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-x C-s") 'editsym--done)
    (define-key map (kbd "C-c C-k") 'editsym--quit)
    (define-key map (kbd "C-c C-q") 'editsym--quit)
	  (define-key map (kbd "C-c C-c") 'editsym--done-quit)
	  map))

(define-derived-mode editsym-mode emacs-lisp-mode "Editsym"
	"Major mode to edit a symbol.")

;;;; epilogue


(provide 'editsym)
;;; editsym.el ends here
