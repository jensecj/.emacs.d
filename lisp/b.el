;;; b.el --- Buffer extentions. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL:
;; Keywords: buffers
;; Package-Requires ((emacs "28.0.50"))
;; Package-Version: 20200429
;; Version: 0.1.0


;;; Commentary:
;;

;;; Code:

(defun b-get-buffer-file-name+ext (&optional buffer)
  "Get the file name and extension of the file belonging to the
current buffer."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer))
      (file-name-nondirectory buffer-file-name))))

(defun b-get-buffer-file-name (&optional buffer)
  "Get the file name of the file belonging to the current
buffer."
  (file-name-sans-extension (b-get-buffer-file-name+ext buffer)))

(defun b-get-buffers-by-name (name)
  "Return all buffers where buffer-name matches NAME."
  (-filter (lambda (b) (s-match name (buffer-name b))) (buffer-list)))

(defun b-get-empty-scratch-buffer ()
  "Return an empty scratch buffer if one exists."
  (let* ((buf-regex (rx bol "*scratch*" (optional "<" (1+ digit)) ">"))
         (bufs (b-get-buffers-by-name buf-regex))
         (empty-bufs (-filter (lambda (b) (< (buffer-size b) 100)) bufs)))
    (first empty-bufs)))

(defun b-get-buffer-create-uniq (name)
  "Return a new buffer with NAME, uniq-ifying its name if needed."
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname
                   (concat name
                           (if (= n 0) "" (format "<%s>" (int-to-string n)))))
             (setq n (1+ n))
             (get-buffer bufname)))
    (get-buffer-create bufname)))

(defun b-new-scratch-buffer ()
  "Return a newly created scratch buffer, uniq-ifying name if needed."
  (b-get-buffer-create-uniq "*scratch*"))

(defun b-jump-to-empty-scratch-buffer ()
  "Create a new scratch buffer to work in. (named *scratch* - *scratch<n>*)."
  (interactive)
  (let ((scratch-buf (or (b-get-empty-scratch-buffer)
                         (b-new-scratch-buffer)))
        (initial-content (if (use-region-p)
                             (buffer-substring (region-beginning) (region-end)))))
    (xref-push-marker-stack)
    (switch-to-buffer scratch-buf)
    (when initial-content (insert initial-content))
    (goto-char (point-min))
    (emacs-lisp-mode)))


(provide 'b)
;;; b.el ends here
