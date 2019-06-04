;;; -*- lexical-binding: t -*-

;; precompute one big autoloads file, makes loading them faster
(setq package-quickstart t)

;; try to minimize garbage-collection during initialization. And disable file
;; visiting modes autoloading when visiting init-files.
(let ((normal-gc-cons-threshold (* 30 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (normal-gc-cons-percentage 0.1)
      (init-gc-cons-percentage 0.7)
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
