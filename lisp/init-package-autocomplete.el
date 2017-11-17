(require 'auto-complete)
(require 'auto-complete-config)

(diminish 'abbrev-mode)

(defun ac-quick-help-at-point ()
  (interactive)
  (let* ((position (point))
         (string-under-cursor
          (buffer-substring-no-properties
           (progn (skip-syntax-backward "w_") (point))
           (progn (skip-syntax-forward "w_") (point)))))
    (goto-char position)
    (popup-tip (ac-symbol-documentation (intern string-under-cursor)))))

(defun my-ac-c++-mode-setup ()
  ;; (require 'ac-clang)
  ;; (require 'ac-c-headers)
  (require 'ac-rtags)

  (setq c++-include-files
        '("/usr/include"
          "/usr/include/c++/7.2.0"
          "/usr/include/c++/7.2.0/backward"
          "/usr/include/c++/7.2.0/x86_64-unknown-linux-gnu"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.2.0/include"
          "/usr/lib/gcc/x86_64-unknown-linux-gnu/7.2.0/include-fixed"
          "/usr/lib/clang/5.0.0/include"))

  (setq-default achead:include-directories c++-include-files)

  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-rtags)
  ;; (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; (add-to-list 'ac-sources 'ac-source-c-header-symbols t)

  ;; (add-to-list 'ac-sources 'ac-source-clang)
  ;; (setq ac-clang-flags (mapcar (lambda (item)(concat "-I" item)) c++-include-files))
  ;; (ac-clang-activate-after-modify)
  )
(add-hook 'c++-mode-hook 'my-ac-c++-mode-setup)

(defun my-ac-elisp-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-functions) ;; elisp functions
  (add-to-list 'ac-sources 'ac-source-features) ;; elisp features
  (add-to-list 'ac-sources 'ac-source-symbols) ;; elisp symbols
  (add-to-list 'ac-sources 'ac-source-variables)) ;; elisp variables
(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)

;; (defun my-ac-latex-mode-setup ()
;;   (require 'auto-complete-auctex)
;;   (require 'ac-auctex-setup))
;; (add-hook 'latex-mode-hook 'my-ac-latex-mode-setup)

(defun my-ac-octave-mode-setup ()
  (require 'ac-octave)
  (add-to-list 'ac-sources 'ac-complete-octave))
(add-hook 'octave-mode-hook 'my-ac-octave-mode-setup)

(provide 'init-package-autocomplete)
