(defun get-file-name+ext ()
  (file-name-nondirectory buffer-file-name))

(defun get-file-name ()
  (file-name-sans-extension (get-file-name+ext)))

(defun get-file-directory ()
  (file-name-directory (buffer-file-name)))

;; use 'C-c C-c' to compile across languages, and use a proper compile command
(add-hook 'c++-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (format "clang++ -std=c++11 %s -o %s" (get-file-name+ext) (get-file-name)))
             (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'java-mode-hook
          '(lambda ()
             (use-local-map nil)
             (set (make-local-variable 'compile-command)
                  (format "javac %s" (get-file-name+ext)))
             (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'csharp-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (format "xbuild %s" (file-name-directory (buffer-file-name))))
             (local-set-key (kbd "C-c C-c") 'compile)))


;; associate file names with modes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(provide 'init-modes)
