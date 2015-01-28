;; Make the mode-line flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; Group colors
(defface face-light '((t (:background "grey35" :inherit mode-line))) "" :group 'powerline)
(defface face-dark '((t (:background "grey30" :inherit mode-line))) "" :group 'powerline)
(defface face-darker '((t (:background "grey25" :inherit mode-line))) "" :group 'powerline)
(defface face-darkest '((t (:background "grey20" :inherit mode-line))) "" :group 'powerline)

;; Setup the powerline theme
(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* (
                        (active (powerline-selected-window-active))
                        (mode-line (if active 'mode-line 'mode-line-inactive))

                        (face-light 'face-light)
                        (face-dark 'face-dark)
                        (face-darker 'face-darker)
                        (face-darkest 'face-darkest)

                        (seperator-> (intern (format "powerline-%s-%s"
                                                     powerline-default-separator
                                                     (car powerline-default-separator-dir))))

                        (separator-< (intern (format "powerline-%s-%s"
                                                     powerline-default-separator
                                                     (cdr powerline-default-separator-dir))))

                        (lhs (list
                              (powerline-buffer-id face-darkest 'l)
                              (powerline-raw " " face-darkest)

                              (funcall seperator-> face-darkest face-darker)

                              (powerline-raw "%4l" face-darker 'r)
                              (powerline-raw ":" face-darker 'l)
                              (powerline-raw "%3c " face-darker 'r)

                              (funcall seperator-> face-darker face-dark)

                              (powerline-major-mode face-dark 'l)
                              (powerline-process face-dark)
                              (powerline-minor-modes face-dark 'l)
                              (powerline-narrow face-dark 'l)

                              (powerline-raw " " face-dark)

                              (funcall seperator-> face-dark face-light)
                              ))

                        (rhs (list
                              (funcall separator-< face-light face-darkest)
                              (powerline-vc face-darkest)
                              )))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill face-light (powerline-width rhs))
                    (powerline-render rhs))))))

(provide 'init-package-powerline)
