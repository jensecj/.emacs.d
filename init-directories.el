;; Define our directories
(defconst root-dir user-emacs-directory)
(defconst lisp-dir (concat root-dir "lisp/")) ; home brewed lisp things
(defconst modes-dir (concat root-dir "modes/")) ; home brewed modes
(defconst defuns-dir (concat root-dir "defuns/"))

(defconst temp-dir (concat root-dir ".temp/"))
(defconst data-dir (concat root-dir "data/")) ; config and cache files
(defconst backup-dir (concat data-dir "backups/")) ; backups and auto saves

(unless (file-exists-p lisp-dir)
  (make-directory lisp-dir))
(unless (file-exists-p modes-dir)
  (make-directory modes-dir))
(unless (file-exists-p defuns-dir)
  (make-directory defuns-dir))

(unless (file-exists-p temp-dir)
  (make-directory temp-dir))
(unless (file-exists-p data-dir)
  (make-directory data-dir))
(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

;; Add home made things to load-path, defuns are loaded seperately
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path modes-dir)

;; Save backup, auto save and temp files in data folder
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)
(setq temporary-file-directory data-dir)

(require 'tramp)
(setq tramp-persistency-file-name (concat data-dir ".tramp"))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat data-dir ".places"))

(provide 'init-directories)
