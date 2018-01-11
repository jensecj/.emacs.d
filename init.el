;; load and activate default packages
(package-initialize)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "config.org"))

(provide 'init)
