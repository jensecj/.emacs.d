;; Define our directories
(defconst my-emacs-dir user-emacs-directory)
(defconst my-emacs-lisp-dir (concat my-emacs-dir "lisp/")) ; lisp things
(defconst my-emacs-modes-dir (concat my-emacs-dir "modes/")) ; modes
(defconst my-emacs-defuns-dir (concat my-emacs-dir "defuns/")) ; misc. functions

(defconst my-emacs-temp-dir (concat my-emacs-dir ".temp/"))
(defconst my-emacs-data-dir (concat my-emacs-dir "data/")) ; config and cache files
(defconst my-emacs-backup-dir (concat my-emacs-data-dir "backups/")) ; backups and auto saves

(unless (file-exists-p my-emacs-lisp-dir)
  (make-directory my-emacs-lisp-dir))
(unless (file-exists-p my-emacs-modes-dir)
  (make-directory my-emacs-modes-dir))
(unless (file-exists-p my-emacs-defuns-dir)
  (make-directory my-emacs-defuns-dir))

(unless (file-exists-p my-emacs-temp-dir)
  (make-directory my-emacs-temp-dir))
(unless (file-exists-p my-emacs-data-dir)
  (make-directory my-emacs-data-dir))
(unless (file-exists-p my-emacs-backup-dir)
  (make-directory my-emacs-backup-dir))

;; Add homemade things to load-path, defuns are loaded seperately
(add-to-list 'load-path my-emacs-lisp-dir)
(add-to-list 'load-path my-emacs-modes-dir)

;; Save backup, auto save files in data folder
(setq backup-directory-alist `((".*" . ,my-emacs-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-emacs-backup-dir t)))
(setq auto-save-list-file-prefix my-emacs-backup-dir)

;; Save temp files in .temp folder
(setq temporary-file-directory my-emacs-temp-dir)

(require 'tramp)
(setq tramp-persistency-file-name (concat my-emacs-data-dir "tramp"))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat my-emacs-data-dir "saveplaces"))

(provide 'init-directories)
