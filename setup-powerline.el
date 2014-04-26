;; Setup the powerline theme
(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* (
                        (active (powerline-selected-window-active))
                        (mode-line (if active 'mode-line 'mode-line-inactive))
                        (face-dark 'powerline-active1)

                        (seperator-> (intern (format "powerline-%s-%s"
                                                     powerline-default-separator
                                                     (car powerline-default-separator-dir))))

                        (separator-< (intern (format "powerline-%s-%s"
                                                     powerline-default-separator
                                                     (cdr powerline-default-separator-dir))))

                        (lhs (list
                              (powerline-buffer-id nil 'l)
                              (powerline-raw " ")

                              (funcall seperator-> mode-line face-dark)
                              (powerline-raw "%4l" face-dark 'r)
                              (powerline-raw ":" face-dark 'l)
                              (powerline-raw "%3c " face-dark 'r)

                              (funcall seperator-> face-dark mode-line)

                              (powerline-major-mode mode-line 'l)
                              (powerline-process mode-line)
                              (powerline-minor-modes mode-line 'l)
                              (powerline-narrow mode-line 'l)

                              (powerline-raw " " mode-line)

                              (funcall seperator-> mode-line face-dark)
                              ))

                        (rhs (list
                              (powerline-raw global-mode-string mode-line 'r)
                              (funcall separator-< face-dark mode-line)
                              (powerline-vc mode-line)
                              )))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill face-dark (powerline-width rhs))
                    (powerline-render rhs))))))

(provide 'setup-powerline)
