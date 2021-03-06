;;; -*- lexical-binding: t -*-

;; turn off excess interface elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)
(tab-line-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; precompute one big autoloads file, makes loading them faster
(setq package-quickstart t)

;; dont load package.el
(setq package-enable-at-startup nil)

;; don't show inscructions on who to exit new frames
(setq server-client-instructions nil)

;; ignore the systems .Xresources
(advice-add #'x-apply-session-resources :override #'ignore)

;; try to minimize garbage-collection during initialization. And disable file
;; visiting modes autoloading when visiting init-files.
(let ((normal-gc-cons-threshold (* 30 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024))
      (normal-gc-cons-percentage 0.1)
      (init-gc-cons-percentage 0.75)
      (normal-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq gc-cons-percentage init-gc-cons-percentage)
  (setq file-name-handler-alist nil)

  ;; reset the things we disabled earlier. set garbage collection limit a bit
  ;; higher, this takes fiddling, because if we set it too low, we end up
  ;; collection all the time, if its too high, each collection will take longer
  ;; to complete.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold)
              (setq gc-cons-percentage normal-gc-cons-percentage)
              (setq file-name-handler-alist normal-file-name-handler-alist))))


(provide 'early-init)
;;; early-init.el ends here
