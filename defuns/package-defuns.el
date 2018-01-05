;; a place for functions I wish were included in their respective packages

(defun counsel-read-file-name (prompt &optional initial-input)
  "Find a file path using ivy-read"
  (interactive)
  (ivy-read prompt 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (if (and counsel-find-file-speedup-remote
                         (file-remote-p ivy--directory))
                    (let ((find-file-hook nil))
                      (expand-file-name x ivy--directory))
                  (expand-file-name x ivy--directory))))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-read-find-name))

(defadvice magit-status (around magit-fullscreen activate)
  "Saves window configuration, then opens magit in fullscreen"
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  ;; only kill the buffer if it's the actual buffer, this way we can
  ;; still get back to our previous configuration if we quit magit weirdly
  (if (s-prefix? "*magit:" (buffer-name (current-buffer)))
      (kill-buffer))
  (jump-to-register :magit-fullscreen))

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

(defvar ivy-save-file (concat my-emacs-data-dir "ivy-views")
  "The file on disk used to save ivy-views")

(defun ivy-save-views ()
  "Save ivy-views to disk"
  (interactive)
  (save-to-file ivy-views ivy-save-file))

;; save ivy-views when pushing/popping views
(advice-add 'ivy-push-view :after #'ivy-save-views)
(advice-add 'ivy-pop-view :after #'ivy-save-views)

(defun ivy-load-views ()
  "Load ivy-views from disk"
  (interactive)
  (setq ivy-views (load-from-file ivy-save-file)))

;; use an empty string as the default view name, instead of buffers
(defun ivy-empty-view-name ()
  "Default name for a new view, used in push-view prompt."
  '"{} ")

;; replace the default view-name
(advice-add 'ivy-default-view-name :override #'ivy-empty-view-name)
