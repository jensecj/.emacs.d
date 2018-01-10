(defvar ivy-save-file (concat my-emacs-data-dir "ivy-views")
  "The file on disk used to save ivy-views")

(defun ivy-save-views ()
  "Save ivy-views to disk"
  (interactive)
  (save-to-file ivy-views ivy-save-file))

;; save ivy-views when pushing/popping views
;; (advice-add 'ivy-push-view :after #'ivy-save-views)
(advice-add 'ivy-pop-view :after #'ivy-save-views)

(defun ivy-load-views ()
  "Load ivy-views from disk"
  (interactive)
  (setq ivy-views (load-from-file ivy-save-file)))

;; use an empty string as the default view name, instead of buffers
(defun ivy-empty-default-view-name ()
  "Default name for a new view, used in push-view prompt."
  '"{} ")

(defun ivy-views-find (view)
  "Find a view from its name"
  (dolist (v ivy-views)
    (if (string= view (car v))
        (return v))))

;; replace the default view-name
(advice-add 'ivy-default-view-name :override #'ivy-empty-default-view-name)
;; (advice-remove 'ivy-default-view-name #'ivy-empty-default-view-name)

(defun my-ivy-push-view ()
  "Push the current window tree on `ivy-views'.
Currently, the split configuration (i.e. horizonal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
  (interactive)
  (let* ((view (cl-labels
                   ((ft (tr)
                        (if (consp tr)
                            (if (eq (car tr) t)
                                (cons 'vert
                                      (mapcar #'ft (cddr tr)))
                              (cons 'horz
                                    (mapcar #'ft (cddr tr))))
                          (with-current-buffer (window-buffer tr)
                            (cond ((buffer-file-name)
                                   (list 'file (buffer-file-name) (point)))
                                  ((eq major-mode 'dired-mode)
                                   (list 'file default-directory (point)))
                                  (t
                                   (list 'buffer (buffer-name) (point))))))))
                 (ft (car (window-tree)))))
         (view-name (ivy-read "Name view: " ivy-views
                              :initial-input (ivy-default-view-name))))
    (when view-name
      ;; pop the view if it already exists, so we replace it
      (ivy-pop-view-action (ivy-views-find view-name))
      (push (list view-name view) ivy-views)))
  ;; save views to file after we're done
  (ivy-save-views))

(advice-add 'ivy-push-view :override #'my-ivy-push-view)

(provide 'init-package-ivy)
