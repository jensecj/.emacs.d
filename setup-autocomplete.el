(require 'auto-complete)
(require 'auto-complete-config)

;; use C-<tab> for completion
(define-key ac-mode-map [(control tab)] 'auto-complete)

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))

(setq-default ac-auto-start t) ;; auto start completing
(setq-default ac-auto-show-menu t) ;; show the menu instantly
(setq-default ac-comphist-file "~/.emacs.d/data/.ac-comphist") ;; move the history file
(setq-default ac-use-fuzzy t) ;; use fuzzy matching
(setq-default ac-quick-help-delay 0.1) ;; show help quickly
(setq-default ac-show-menu-immediately-on-auto-complete t)

;; set face colors
(set-face-background 'ac-candidate-face "#3E3E3E")
(set-face-foreground 'ac-candidate-face "#DCDCCC")
(set-face-background 'ac-selection-face "#313131")
(set-face-foreground 'ac-selection-face "#FEFEFE")

(global-auto-complete-mode t)

(defun my-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  (require 'auto-complete-c-headers)

  (setq cc-include-files
        '("/usr/include"
          "/usr/local/include"
          "/usr/include/c++/4.9.0"
          "/usr/include/c++/4.9.0/backward"
          "/usr/include/c++/4.9.0/x86_64-unknown-linux-gnu"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.0/include"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.0/include-fixed"
          ))

  (setq company-clang-arguments cc-include-files)

  (setq ac-clang-flags (mapcar (lambda (item)(concat "-I" item)) cc-include-files))
  (setq achead:include-directories cc-include-files)

  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-clang)
  (add-to-list 'ac-sources 'ac-source-c-headers)

  ;; set face colors... again
  (set-face-background 'ac-clang-candidate-face "#3E3E3E")
  (set-face-foreground 'ac-clang-candidate-face "#DCDCCC")
  (set-face-background 'ac-clang-selection-face "#313131")
  (set-face-foreground 'ac-clang-selection-face "#FEFEFE")
  )

(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)

(defun my-ac-elisp-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-functions) ;; elisp functions
  (add-to-list 'ac-sources 'ac-source-features) ;; (require '
  (add-to-list 'ac-sources 'ac-source-symbols) ;; elisp symbols
  (add-to-list 'ac-sources 'ac-source-variables)) ;; elisp variables

(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)


(provide 'setup-autocomplete)
