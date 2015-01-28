;; load all files in defuns-dir
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(provide 'init-defuns)
