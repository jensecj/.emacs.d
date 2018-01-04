;; load all files in my-emacs-defuns-dir
(dolist (file (directory-files my-emacs-defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(provide 'init-defuns)
