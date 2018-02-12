;; load and activate default packages
(package-initialize)

;; include the commomn lisp functions
(require 'cl)

(message (format "= \e[1m\e[32mStarted initializing emacs!\e[0m"))

;; setup package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(defconst melpa-archive (concat user-emacs-directory "elpa/archives/melpa"))

;; install use-package if we don't already have it
(unless (package-installed-p 'use-package)
  (message (format "= \e[1m\e[31muse-package is not installed! installing...\e[0m"))
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package tell us what its doing
(setq use-package-verbose t)

;; I want to use the new version of org-mode from upstream.
;; remove the built-in org-mode from the load path, so it does not get loaded
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; remove org-mode from the built-ins list, because we're using upstream
(setq package--builtins (assq-delete-all 'org package--builtins))

;; install upstream org-mode
(use-package org :ensure org-plus-contrib :pin org)

(org-babel-load-file (concat user-emacs-directory "config.org"))

(message (format "= \e[1m\e[32mEmacs initialized in %s\e[0m" (emacs-init-time)))

(provide 'init)
