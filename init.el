;; load default packages
(package-initialize)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Add the emacs directory to the load path
(add-to-list 'load-path user-emacs-directory)

(defvar init-files
  '(
    init-keybindings ;; Setup keybindings for everything
    init-modes ;; Setup mode specific configuration
    init-tty ;; terminal specific things
    ))

(defun try-require (feature)
  "Tries to require FEATURE, if an exception is thrown, log it."
  (condition-case ex
      (progn
        (message (format "@ \e[94m Loading \"%s\" \e[0m" (symbol-name feature)))
        (require feature))
    ('error (message (format "@ \e[1m\e[31m Error loading \"%s\": %s \e[0m" (symbol-name feature) ex)))))

;; Safely load all the init files
(message "# \e[96m Started loading init files \e[0m")
(dolist (file init-files)
  (try-require file))
(message "# \e[96m Finished loading init files \e[0m")

(provide 'init)
