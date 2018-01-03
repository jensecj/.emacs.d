(package-initialize)

;; Add the emacs directory to the load path
(add-to-list 'load-path user-emacs-directory)

(defvar init-files
  '(
    init-directories ;; Setup directories and add things to the load-path
    init-defuns ;; Load all home made lisp functions
    init-packages ;; Handle missing packages, then package specific configuration
    ;; init-el-get ;; Handle packages managed by el-get
    init-defaults ;; Setup some better defaults
    init-modes ;; Setup mode specific configuration
    init-keybindings ;; Setup keybindings for everything
    init-experimental ;; Initialize things that are works in progress
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

(message (format "= \e[1m\e[32mEmacs initialized in %s\e[0m" (emacs-init-time)))

(provide 'init)
