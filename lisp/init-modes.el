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
                  (format "clang++ -std=c++17 -stdlib=libstdc++ %s -o %s" (get-file-name+ext) (get-file-name)))
             (local-set-key (kbd "C-c C-c") 'compile)
             (local-set-key (kbd "C-c n") (clang-format-buffer))))

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

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (use-local-map nil)
             (set (make-local-variable 'compile-command)
                  (format "ocamlopt -o %s %s" (get-file-name) (get-file-name+ext)))
             (local-set-key (kbd "C-c C-c") 'compile)))

(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "<S-up>") 'windmove-up)
             (local-set-key (kbd "<S-down>") 'windmove-down)
             (local-set-key (kbd "<S-left>") 'windmove-left)
             (local-set-key (kbd "<S-right>") 'windmove-right)))


;; use chicken scheme for scheme things
(setq scheme-program-name "csi -:c")

;; associate file names with modes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(provide 'init-modes)
