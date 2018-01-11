;; load default packages
(package-initialize)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "config.org"))

(defun try-require (feature)
  "Tries to require FEATURE, if an exception is thrown, log it."
  (condition-case ex
      (progn
        (message (format "@ \e[94m Loading \"%s\" \e[0m" (symbol-name feature)))
        (require feature))
    ('error (message (format "@ \e[1m\e[31m Error loading \"%s\": %s \e[0m" (symbol-name feature) ex)))))

;; Safely load all the init files
(message "# \e[96m Started loading init files \e[0m")
(try-require 'init-keybindings)
(message "# \e[96m Finished loading init files \e[0m")

(provide 'init)
