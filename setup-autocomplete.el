(require 'auto-complete)
(require 'auto-complete-config)

;; hide auto completion modes from the minibuffers list
(diminish 'auto-complete-mode)
(diminish 'abbrev-mode)

(setq-default ac-sources '(ac-source-words-in-buffer ac-source-yasnippet))

(setq-default ac-auto-start t) ;; auto start completing
(setq-default ac-auto-show-menu t) ;; show the menu instantly
(setq-default ac-show-menu-immediately-on-auto-complete t) ;; show the autocompletion menu instantly
(setq-default ac-delay 0.1) ;; show completion menu quickly
(setq-default ac-use-quick-help t) ;; use the help
(setq-default ac-quick-help-delay 0.1) ;; show help quickly
(setq-default ac-comphist-file "~/.emacs.d/data/.ac-comphist") ;; move the history file

;; set face colors
(setq candidate-face-fg "#F0DFAF")
(setq candidate-face-bg "#313131")
(setq selection-face-fg "#FEFEFE")
(setq selection-face-bg "#3E3E3E")

(set-face-background 'ac-candidate-face candidate-face-bg)
(set-face-foreground 'ac-candidate-face candidate-face-fg)
(set-face-background 'ac-selection-face selection-face-bg)
(set-face-foreground 'ac-selection-face selection-face-fg)

(global-set-key (kbd "C-<tab>") 'auto-complete)

(global-auto-complete-mode t)

(defun my-ac-c++-mode-setup ()
  (require 'auto-complete-clang)
  (require 'auto-complete-c-headers)

  (setq c++-include-files
        '("/usr/include"
          "/usr/local/include"
          "/usr/include/c++/4.9.0"
          "/usr/include/c++/4.9.0/backward"
          "/usr/include/c++/4.9.0/x86_64-unknown-linux-gnu"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.0/include"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.0/include-fixed"
          ))

  (setq-default achead:include-directories c++-include-files)

  (setq ac-clang-flags (mapcar (lambda (item)(concat "-I" item)) c++-include-files))

  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-c-headers)

  (set-face-background 'ac-clang-candidate-face candidate-face-bg)
  (set-face-foreground 'ac-clang-candidate-face candidate-face-fg)
  (set-face-background 'ac-clang-selection-face selection-face-bg)
  (set-face-foreground 'ac-clang-selection-face selection-face-fg)

  (local-set-key (kbd "C-<tab>") 'ac-complete-clang)
  )

(add-hook 'c++-mode-hook 'my-ac-c++-mode-setup)

(defun my-ac-elisp-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-functions) ;; elisp functions
  (add-to-list 'ac-sources 'ac-source-features) ;; elisp features
  (add-to-list 'ac-sources 'ac-source-symbols) ;; elisp symbols
  (add-to-list 'ac-sources 'ac-source-variables) ;; elisp variables
  )

(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)

(provide 'setup-autocomplete)
