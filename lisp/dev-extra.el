(require 'lisp-mnt)
(require 's)
(require 'dash)

(defun dev--header-exists (header)
  "Return content of header if it exists."
  (save-excursion
    (goto-char (point-min))
    (lm-header header)))

(defun dev--crack-version (ver)
  "Convert a version string into a list of numbers."
  (->> ver
       (s-split (regexp-quote "."))
       (-map #'string-to-number)))

(defun dev--bump-version (ver loc)
  "Bump the LOC part of version VER."
  (when-let* ((ver (dev--crack-version ver)))
    ;; make sure the version is in three parts (major.minor.patch)
    (while (< (length ver) 3)
      (setq ver (-snoc ver 0)))

    (cond
     ((equal loc 'patch)
      (message (format "%s.%s.%s" (nth 0 ver) (nth 1 ver) (1+ (nth 2 ver)))))
     ((equal loc 'minor)
      (message (format "%s.%s.%s" (nth 0 ver) (1+ (nth 1 ver)) 0)))
     ((equal loc 'major)
      (message (format "%s.%s.%s" (1+ (nth 0 ver)) 0 0))))))

(defun dev--update-version (ver)
  "Update the package version to VER."
  (save-excursion
    (lm-header "version")
    (kill-line)
    (insert ver)))

(defun dev-bump-patch ()
  "Bump the patch part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'patch))))

(defun dev-bump-minor ()
  "Bump the minor part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'minor))))

(defun dev-bump-major ()
  "Bump the major part of the version header."
  (interactive)
  (when-let ((ver (dev--header-exists "version")))
    (dev--update-version (dev--bump-version ver 'major))))

(defun dev-update-package-version ()
  "Update the package-version to current date in compact format."
  (interactive)
  (when (dev--header-exists "package-version")
    (save-excursion
      (lm-header "package-version")
      (kill-line)
      (insert (format-time-string "%Y%m%d")))))

(provide 'dev-extra)
