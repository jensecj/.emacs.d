(require 'multi-term)

(defvar multi-terms '()
  "List of saved terminals")
(defvar multi-term-save-file (concat my-emacs-data-dir "multi-terms")
  "File on disk used to store the list of saved terminals")

(defun better-multi-term (&optional stay-in-buffer)
  "Create new term buffer."
  (interactive)
  (let ((term-buffer)
        (buffer-new-name (concat "*" default-directory "*")))
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (if (not stay-in-buffer)
        (switch-to-buffer term-buffer))
    (rename-buffer buffer-new-name)))

(defun multi-term-save-term ()
  "Pick an open terminal and save it"
  (interactive)
  (if (null multi-term-buffer-list)
      (error "Error: No open terminals."))
  (let ((buf (get-buffer (ivy-read "Select term:" (mapcar 'buffer-name multi-term-buffer-list)))))
    (with-current-buffer buf
      (if (member default-directory multi-terms)
          (error "That term is already saved"))
      (add-to-list 'multi-terms default-directory)))
  (save-to-file multi-terms multi-term-save-file))

(defun multi-term-unsave-term ()
  "Pick a saved terminal to remove from the saved list"
  (interactive)
  (let ((trm (ivy-read "Select term:" multi-terms)))
    (setq multi-terms (delete trm multi-terms)))
  (save-to-file multi-terms multi-term-save-file))

(defun multi-term-restore-terms ()
  "Restores all terminals from the saved list"
  (interactive)
  (setq multi-terms (load-from-file multi-term-save-file))
  (ignore-errors
    (dolist (trm multi-terms)
      (let ((default-directory trm))
        (better-multi-term t)))))

(defun multi-term-list-saves ()
  "List all saved terminals"
  (interactive)
  (ivy-read "All saved terms:" (load-from-file multi-term-save-file)))

;; restore all saved terminals at startup
(multi-term-restore-terms)

(provide 'init-package-multi-term)
