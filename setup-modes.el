(defun my-c++-mode-setup ()
  (setq compile-command
        (format "clang++ -std=c++11 %s -o %s"
                (file-name-nondirectory buffer-file-name)
                (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))

(add-hook 'c++-mode-hook 'my-c++-mode-setup)

(provide 'setup-modes)
