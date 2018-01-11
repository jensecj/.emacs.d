;; load and activate default packages
(package-initialize)

;; setup package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; install use-package if we don't already have it
(unless (package-installed-p 'use-package)
  (message (format "= \e[1m\e[31muse-package is not installed! installing...\e[0m"))
  (package-refresh-contents)
  (package-install 'use-package))

;; I want to use the new version of org-mode from upstream:

;; remove the built-in org-mode from the load path, so it does not get loaded
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; remove org-mode from the built-ins list, because we're using upstream
(setq package--builtins (assq-delete-all 'org package--builtins))

;; install upstream org-mode
(use-package org :ensure t :pin org)

(org-babel-load-file (concat user-emacs-directory "config.org"))

(provide 'init)
